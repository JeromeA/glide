#include "real_glide_session.h"
#include "util.h"

static gpointer real_glide_session_thread(gpointer data);
static void real_glide_session_eval(GlideSession *session, Interaction *interaction);
static void real_glide_session_set_added(GlideSession *session, GlideSessionCallback cb, gpointer user_data);
static void real_glide_session_set_updated(GlideSession *session, GlideSessionCallback cb, gpointer user_data);
static void real_glide_session_destroy(GlideSession *session);

static const GlideSessionOps real_glide_session_ops = {
  .eval = real_glide_session_eval,
  .set_interaction_added_cb = real_glide_session_set_added,
  .set_interaction_updated_cb = real_glide_session_set_updated,
  .destroy = real_glide_session_destroy,
};

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

GlideSession *real_glide_session_new(GlideProcess *proc, StatusService *status_service) {
  g_debug("RealGlideSession.new");
  RealGlideSession *self = g_new0(RealGlideSession, 1);
  self->base.ops = &real_glide_session_ops;
  self->base.refcnt = 1;
  self->proc = proc ? glide_process_ref(proc) : NULL;
  self->status_service = status_service;
  self->started = FALSE;
  self->queue = g_async_queue_new();
  self->thread = g_thread_new("glide-session", real_glide_session_thread, self);
  g_mutex_init(&self->lock);
  g_cond_init(&self->cond);
  self->current = NULL;
  self->added_cb = NULL;
  self->added_cb_data = NULL;
  self->updated_cb = NULL;
  self->updated_cb_data = NULL;
  if (self->proc)
    glide_process_set_message_cb(self->proc, real_glide_session_on_message, self);
  return (GlideSession*)self;
}

static gpointer real_glide_session_thread(gpointer data) {
  RealGlideSession *self = data;
  for (;;) {
    gpointer item = g_async_queue_pop(self->queue);
    if (item == GINT_TO_POINTER(1))
      break;
    Interaction *interaction = item;
    g_mutex_lock(&self->lock);
    if (!self->started) {
      guint status_id = status_service_publish(self->status_service, "SBCL is starting...");
      glide_process_start(self->proc);
      status_service_unpublish(self->status_service, status_id);
      self->started = TRUE;
    }
    interaction->status = INTERACTION_RUNNING;
    self->current = interaction;
    GlideSessionCallback added_cb = self->added_cb;
    gpointer added_cb_data = self->added_cb_data;
    g_mutex_unlock(&self->lock);
    if (added_cb)
      added_cb((GlideSession*)self, interaction, added_cb_data);
    gchar *escaped = escape_string(interaction->expression);
    gchar *cmd = g_strdup_printf("(glide:glide-eval \"(let ((*debugger-hook* nil)) %s)\\n\")\n", escaped);
    GString *payload = g_string_new(cmd);
    g_free(escaped);
    g_free(cmd);
    glide_process_send(self->proc, payload);
    g_string_free(payload, TRUE);
    g_mutex_lock(&self->lock);
    while (self->current)
      g_cond_wait(&self->cond, &self->lock);
    g_mutex_unlock(&self->lock);
  }
  return NULL;
}

static void real_glide_session_eval(GlideSession *session, Interaction *interaction) {
  RealGlideSession *self = (RealGlideSession*)session;
  if (self->queue)
    g_async_queue_push(self->queue, interaction);
}

static void real_glide_session_set_added(GlideSession *session, GlideSessionCallback cb, gpointer user_data) {
  RealGlideSession *self = (RealGlideSession*)session;
  self->added_cb = cb;
  self->added_cb_data = user_data;
}

static void real_glide_session_set_updated(GlideSession *session, GlideSessionCallback cb, gpointer user_data) {
  RealGlideSession *self = (RealGlideSession*)session;
  self->updated_cb = cb;
  self->updated_cb_data = user_data;
}

static void real_glide_session_destroy(GlideSession *session) {
  RealGlideSession *self = (RealGlideSession*)session;
  if (self->queue)
    g_async_queue_push(self->queue, GINT_TO_POINTER(1));
  if (self->thread)
    g_thread_join(self->thread);
  if (self->proc)
    glide_process_unref(self->proc);
  if (self->queue)
    g_async_queue_unref(self->queue);
  g_mutex_clear(&self->lock);
  g_cond_clear(&self->cond);
  g_free(self);
}

void real_glide_session_on_message(GString *msg, gpointer user_data) {
  RealGlideSession *self = user_data ? (RealGlideSession*)user_data : NULL;
  const char *str = msg->str;
  g_debug("RealGlideSession.on_message %s", str);
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
      interaction->result = g_strdup(res);
      interaction->status = INTERACTION_OK;
      GlideSessionCallback updated_cb = self->updated_cb;
      gpointer updated_cb_data = self->updated_cb_data;
      InteractionCallback done_cb = interaction->done_cb;
      gpointer done_cb_data = interaction->done_cb_data;
      self->current = NULL;
      g_mutex_unlock(&self->lock);
      if (updated_cb) {
        g_debug("RealGlideSession.on_message invoking updated_cb");
        updated_cb((GlideSession*)self, interaction, updated_cb_data);
      }
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
      interaction->error = g_strdup(err);
      interaction->status = INTERACTION_ERROR;
      GlideSessionCallback updated_cb = self->updated_cb;
      gpointer updated_cb_data = self->updated_cb_data;
      InteractionCallback done_cb = interaction->done_cb;
      gpointer done_cb_data = interaction->done_cb_data;
      self->current = NULL;
      g_mutex_unlock(&self->lock);
      if (updated_cb) {
        g_debug("RealGlideSession.on_message invoking updated_cb");
        updated_cb((GlideSession*)self, interaction, updated_cb_data);
      }
      if (done_cb)
        done_cb(interaction, done_cb_data);
      g_mutex_lock(&self->lock);
      g_cond_broadcast(&self->cond);
    }
  }
  g_mutex_unlock(&self->lock);
}

