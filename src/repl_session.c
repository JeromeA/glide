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
  GHashTable *interactions;
  ReplSessionCallback added_cb;
  gpointer added_cb_data;
  ReplSessionCallback updated_cb;
  gpointer updated_cb_data;
};

static volatile gint next_tag = 1;

static gpointer repl_session_thread(gpointer data);
static GString *escape_string(const char *str) {
  GString *out = g_string_new(NULL);
  for (const char *p = str; *p; p++) {
    switch (*p) {
      case '\\': g_string_append(out, "\\\\"); break;
      case '"':  g_string_append(out, "\\\""); break;
      default:    g_string_append_c(out, *p);
    }
  }
  return out;
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
  if (self->interactions)
    g_hash_table_destroy(self->interactions);
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
  self->interactions = g_hash_table_new(g_direct_hash, g_direct_equal);
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
    gchar *expr = NULL;
    g_mutex_lock(&interaction->lock);
    expr = interaction->expression ? g_strdup(interaction->expression->str) : NULL;
    g_mutex_unlock(&interaction->lock);
    LOG_LONG(1, "ReplSession.thread eval ", expr);
    g_mutex_lock(&self->lock);
    if (!self->started) {
      repl_process_start(self->proc);
      self->started = TRUE;
    }
    g_mutex_lock(&interaction->lock);
    interaction->status = INTERACTION_RUNNING;
    guint32 tag = interaction->tag;
    g_mutex_unlock(&interaction->lock);
    g_hash_table_insert(self->interactions, GINT_TO_POINTER(tag), interaction);
    ReplSessionCallback added_cb = self->added_cb;
    gpointer added_cb_data = self->added_cb_data;
    g_mutex_unlock(&self->lock);
    if (added_cb)
      added_cb(self, interaction, added_cb_data);
    g_mutex_lock(&interaction->lock);
    GString *escaped = escape_string(interaction->expression->str);
    guint32 cmd_tag = interaction->tag;
    g_mutex_unlock(&interaction->lock);
    GString *payload = g_string_new(NULL);
    g_string_append_printf(payload,
        "(glide:eval-and-capture %u \"%s\")\n", cmd_tag, escaped->str);
    g_string_free(escaped, TRUE);
    g_free(expr);
    LOG_LONG(1, "ReplSession.thread send ", payload->str);
    repl_process_send(self->proc, payload);
    g_string_free(payload, TRUE);
  }
  return NULL;
}

void repl_session_eval(ReplSession *self, Interaction *interaction) {
  g_return_if_fail(self);
  if (self->queue) {
    g_mutex_lock(&interaction->lock);
    interaction->tag = g_atomic_int_add(&next_tag, 1);
    gchar *expr = interaction->expression ? g_strdup(interaction->expression->str) : NULL;
    g_mutex_unlock(&interaction->lock);
    LOG_LONG(1, "ReplSession.eval queue ", expr);
    g_async_queue_push(self->queue, interaction);
    g_free(expr);
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
  LOG_LONG(1, "ReplSession.on_message ", str);
  while (*str && *str != '(') {
    if (*str == ';') {
      const char *nl = strchr(str, '\n');
      if (!nl)
        return;
      str = nl + 1;
    } else {
      str++;
    }
  }
  if (*str != '(')
    return;
  ReplSessionCallback updated_cb = NULL;
  gpointer updated_cb_data = NULL;
  InteractionCallback done_cb = NULL;
  gpointer done_cb_data = NULL;
  Interaction *interaction = NULL;
  gboolean finished = FALSE;
  g_mutex_lock(&self->lock);
  if (g_str_has_prefix(str, "(stdout ")) {
    const char *p = str + strlen("(stdout ");
    gchar *endptr = NULL;
    guint32 id = g_ascii_strtoull(p, &endptr, 10);
    interaction = g_hash_table_lookup(self->interactions, GINT_TO_POINTER(id));
    if (interaction && endptr && *endptr == ' ' && *(endptr + 1) == '"') {
      const char *start = endptr + 2;
      const char *end = strstr(start, "\")");
      if (end) {
        gchar *text = g_strndup(start, end - start);
        LOG_LONG(1, "ReplSession.on_message stdout: ", text);
        g_mutex_lock(&interaction->lock);
        if (!interaction->output)
          interaction->output = g_string_new(text);
        else
          g_string_append(interaction->output, text);
        g_mutex_unlock(&interaction->lock);
        g_free(text);
        updated_cb = self->updated_cb;
        updated_cb_data = self->updated_cb_data;
      }
    }
  } else if (g_str_has_prefix(str, "(stderr ")) {
    const char *p = str + strlen("(stderr ");
    gchar *endptr = NULL;
    guint32 id = g_ascii_strtoull(p, &endptr, 10);
    interaction = g_hash_table_lookup(self->interactions, GINT_TO_POINTER(id));
    if (interaction && endptr && *endptr == ' ' && *(endptr + 1) == '"') {
      const char *start = endptr + 2;
      const char *end = strstr(start, "\")");
      if (end) {
        gchar *text = g_strndup(start, end - start);
        LOG_LONG(1, "ReplSession.on_message stderr: ", text);
        g_mutex_lock(&interaction->lock);
        if (!interaction->output)
          interaction->output = g_string_new(text);
        else
          g_string_append(interaction->output, text);
        g_mutex_unlock(&interaction->lock);
        g_free(text);
        updated_cb = self->updated_cb;
        updated_cb_data = self->updated_cb_data;
      }
    }
  } else if (g_str_has_prefix(str, "(result ")) {
    const char *p = str + strlen("(result ");
    gchar *endptr = NULL;
    guint32 id = g_ascii_strtoull(p, &endptr, 10);
    interaction = g_hash_table_lookup(self->interactions, GINT_TO_POINTER(id));
    if (interaction && endptr && *endptr == ' ') {
      const char *start = endptr + 1;
      const char *end = strrchr(start, ')');
      if (end) {
        gchar *res = g_strndup(start, end - start);
        LOG_LONG(1, "ReplSession.on_message result: ", res);
        g_mutex_lock(&interaction->lock);
        if (interaction->result)
          g_string_assign(interaction->result, res);
        else
          interaction->result = g_string_new(res);
        interaction->status = INTERACTION_OK;
        done_cb = interaction->done_cb;
        done_cb_data = interaction->done_cb_data;
        g_mutex_unlock(&interaction->lock);
        g_free(res);
        updated_cb = self->updated_cb;
        updated_cb_data = self->updated_cb_data;
        finished = TRUE;
      }
    }
  } else if (g_str_has_prefix(str, "(error ")) {
    const char *p = str + strlen("(error ");
    gchar *endptr = NULL;
    guint32 id = g_ascii_strtoull(p, &endptr, 10);
    interaction = g_hash_table_lookup(self->interactions, GINT_TO_POINTER(id));
    if (interaction && endptr && *endptr == ' ' && *(endptr + 1) == '"') {
      const char *start = endptr + 2;
      const char *end = strrchr(start, '"');
      if (end) {
        gchar *err = g_strndup(start, end - start);
        LOG_LONG(1, "ReplSession.on_message error: ", err);
        g_mutex_lock(&interaction->lock);
        if (interaction->error)
          g_string_assign(interaction->error, err);
        else
          interaction->error = g_string_new(err);
        interaction->status = INTERACTION_ERROR;
        updated_cb = self->updated_cb;
        updated_cb_data = self->updated_cb_data;
        done_cb = interaction->done_cb;
        done_cb_data = interaction->done_cb_data;
        g_mutex_unlock(&interaction->lock);
        finished = TRUE;
        g_free(err);
      }
    }
  } else {
    LOG_LONG(1, "ReplSession.on_message unknown message: ", str);
  }
  g_mutex_unlock(&self->lock);
  if (updated_cb)
    updated_cb(self, interaction, updated_cb_data);
  if (finished) {
    if (done_cb)
      done_cb(interaction, done_cb_data);
    g_mutex_lock(&self->lock);
    g_cond_broadcast(&self->cond);
    g_mutex_unlock(&self->lock);
  }
}
