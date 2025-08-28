#include "repl_session.h"
#include "util.h"

#include <string.h>

struct _ReplSession {
  int refcnt;
  ReplProcess *proc;
  StatusService *status_service;
  gboolean started;
  GAsyncQueue *queue;
  GThread *thread;
  GMutex lock;
  GCond cond;
  Interaction *current;
  ReplSessionCallback added_cb;
  gpointer added_cb_data;
  ReplSessionCallback updated_cb;
  gpointer updated_cb_data;
};

static gpointer repl_session_thread(gpointer data);
static gchar *escape_string(const char *str) {
  GString *out = g_string_new(NULL);
  for (const char *p = str; *p; p++) {
    switch (*p) {
      case '\\': g_string_append(out, "\\\\"); break;
      case '"':  g_string_append(out, "\\\""); break;
      default:    g_string_append_c(out, *p);
    }
  }
  return g_string_free(out, FALSE);
}

ReplSession *repl_session_ref(ReplSession *self) {
  g_return_val_if_fail(self, NULL);
  self->refcnt++;
  return self;
}

static void repl_session_destroy(ReplSession *self) {
  if (self->queue)
    g_async_queue_push(self->queue, GINT_TO_POINTER(1));
  if (self->thread)
    g_thread_join(self->thread);
  if (self->proc)
    repl_process_unref(self->proc);
  if (self->queue)
    g_async_queue_unref(self->queue);
  g_mutex_clear(&self->lock);
  g_cond_clear(&self->cond);
  g_free(self);
}

void repl_session_unref(ReplSession *self) {
  if (!self)
    return;
  if (--self->refcnt == 0)
    repl_session_destroy(self);
}

ReplSession *repl_session_new(ReplProcess *proc, StatusService *status_service) {
  ReplSession *self = g_new0(ReplSession, 1);
  self->refcnt = 1;
  self->proc = proc ? repl_process_ref(proc) : NULL;
  self->status_service = status_service;
  self->started = FALSE;
  self->queue = g_async_queue_new();
  self->thread = g_thread_new("repl-session", repl_session_thread, self);
  g_mutex_init(&self->lock);
  g_cond_init(&self->cond);
  self->current = NULL;
  self->added_cb = NULL;
  self->added_cb_data = NULL;
  self->updated_cb = NULL;
  self->updated_cb_data = NULL;
  if (self->proc)
    repl_process_set_message_cb(self->proc, repl_session_on_message, self);
  return self;
}

static gpointer repl_session_thread(gpointer data) {
  ReplSession *self = data;
  for (;;) {
    gpointer item = g_async_queue_pop(self->queue);
    if (item == GINT_TO_POINTER(1))
      break;
    Interaction *interaction = item;
    g_debug("ReplSession.thread eval %s", interaction->expression);
    g_mutex_lock(&self->lock);
    if (!self->started) {
      guint status_id = status_service_publish(self->status_service, "SBCL is starting...");
      repl_process_start(self->proc);
      status_service_unpublish(self->status_service, status_id);
      self->started = TRUE;
    }
    interaction->status = INTERACTION_RUNNING;
    self->current = interaction;
    ReplSessionCallback added_cb = self->added_cb;
    gpointer added_cb_data = self->added_cb_data;
    g_mutex_unlock(&self->lock);
    if (added_cb)
      added_cb(self, interaction, added_cb_data);
    gchar *escaped = escape_string(interaction->expression);
    gchar *cmd = g_strdup_printf("(glide:glide-eval \"%s\")\n", escaped);
    GString *payload = g_string_new(cmd);
    g_free(escaped);
    g_free(cmd);
    g_debug("ReplSession.thread send %s", payload->str);
    repl_process_send(self->proc, payload);
    g_string_free(payload, TRUE);
    g_mutex_lock(&self->lock);
    while (self->current)
      g_cond_wait(&self->cond, &self->lock);
    g_mutex_unlock(&self->lock);
  }
  return NULL;
}

void repl_session_eval(ReplSession *self, Interaction *interaction) {
  g_return_if_fail(self);
  if (self->queue) {
    g_debug("ReplSession.eval queue %s", interaction->expression);
    g_async_queue_push(self->queue, interaction);
  }
}

void repl_session_set_interaction_added_cb(ReplSession *self, ReplSessionCallback cb, gpointer user_data) {
  g_return_if_fail(self);
  self->added_cb = cb;
  self->added_cb_data = user_data;
}

void repl_session_set_interaction_updated_cb(ReplSession *self, ReplSessionCallback cb, gpointer user_data) {
  g_return_if_fail(self);
  self->updated_cb = cb;
  self->updated_cb_data = user_data;
}

void repl_session_on_message(GString *msg, gpointer user_data) {
  ReplSession *self = user_data ? (ReplSession*)user_data : NULL;
  const char *str = msg->str;
  g_debug("ReplSession.on_message %s", str);
  g_mutex_lock(&self->lock);
  Interaction *interaction = self->current;
  if (!interaction) {
    g_mutex_unlock(&self->lock);
    return;
  }
  if (g_str_has_prefix(str, "(stdout \"")) {
    const char *start = str + strlen("(stdout \"");
    const char *end = strstr(start, "\")");
    if (end) {
      gchar *text = g_strndup(start, end - start);
      g_debug("ReplSession.on_message stdout: %s", text);
      gchar *old = interaction->output;
      interaction->output = old ? g_strconcat(old, text, NULL) : g_strdup(text);
      g_free(old);
      g_free(text);
    }
  } else if (g_str_has_prefix(str, "(stderr \"")) {
    const char *start = str + strlen("(stderr \"");
    const char *end = strstr(start, "\")");
    if (end) {
      gchar *text = g_strndup(start, end - start);
      g_debug("ReplSession.on_message stderr: %s", text);
      gchar *old = interaction->output;
      interaction->output = old ? g_strconcat(old, text, NULL) : g_strdup(text);
      g_free(old);
      g_free(text);
    }
  } else if (g_str_has_prefix(str, "(result ")) {
    const char *start = str + strlen("(result ");
    const char *end = strrchr(start, ')');
    if (end) {
      gchar *res = g_strndup(start, end - start);
      g_debug("ReplSession.on_message result: %s", res);
      interaction->result = g_strdup(res);
      interaction->status = INTERACTION_OK;
      ReplSessionCallback updated_cb = self->updated_cb;
      gpointer updated_cb_data = self->updated_cb_data;
      InteractionCallback done_cb = interaction->done_cb;
      gpointer done_cb_data = interaction->done_cb_data;
      self->current = NULL;
      g_mutex_unlock(&self->lock);
      if (updated_cb)
        updated_cb(self, interaction, updated_cb_data);
      if (done_cb)
        done_cb(interaction, done_cb_data);
      g_mutex_lock(&self->lock);
      g_cond_broadcast(&self->cond);
    }
  } else if (g_str_has_prefix(str, "(error \"")) {
    const char *start = str + strlen("(error \"");
    const char *end = strrchr(start, '"');
    if (end) {
      gchar *err = g_strndup(start, end - start);
      g_debug("ReplSession.on_message error: %s", err);
      interaction->error = g_strdup(err);
      interaction->status = INTERACTION_ERROR;
      ReplSessionCallback updated_cb = self->updated_cb;
      gpointer updated_cb_data = self->updated_cb_data;
      InteractionCallback done_cb = interaction->done_cb;
      gpointer done_cb_data = interaction->done_cb_data;
      self->current = NULL;
      g_mutex_unlock(&self->lock);
      if (updated_cb)
        updated_cb(self, interaction, updated_cb_data);
      if (done_cb)
        done_cb(interaction, done_cb_data);
      g_mutex_lock(&self->lock);
      g_cond_broadcast(&self->cond);
    }
  }
  g_mutex_unlock(&self->lock);
}
