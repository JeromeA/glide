#include "real_glide_process.h"
#include "util.h"

#include <string.h>

enum {
  START_STATE_IDLE = 0,
  START_STATE_WAIT_PROMPT,
  START_STATE_WAIT_NIL,
  START_STATE_WAIT_PROMPT2,
  START_STATE_DONE,
};

static void gp_start(GlideProcess *base);
static void gp_send(GlideProcess *base, const GString *payload);
static void gp_set_message_cb(GlideProcess *base, GlideProcessMessageCallback cb,
                              gpointer user_data);
static void gp_destroy(GlideProcess *base);

static const GlideProcessOps real_glide_process_ops = {
  .start = gp_start,
  .send = gp_send,
  .set_message_cb = gp_set_message_cb,
  .destroy = gp_destroy,
};

static void on_proc_out(GString *data, gpointer user_data) {
  g_debug("RealGlideProcess.on_proc_out %s", data->str);
  RealGlideProcess *self = user_data;
  g_mutex_lock(&self->mutex);
  g_string_append_len(self->buffer, data->str, data->len);
  while (TRUE) {
    char *newline = memchr(self->buffer->str, '\n', self->buffer->len);
    if (!newline)
      break;
    gsize len = newline - self->buffer->str;
    GString *line = g_string_new_len(self->buffer->str, len);
    g_string_erase(self->buffer, 0, len + 1);
    if (self->start_state != START_STATE_DONE) {
      if (self->start_state == START_STATE_WAIT_PROMPT && strcmp(line->str, "*") == 0) {
        self->start_state = START_STATE_WAIT_NIL;
        g_cond_broadcast(&self->cond);
      } else if (self->start_state == START_STATE_WAIT_NIL && strcmp(line->str, "NIL") == 0) {
        self->start_state = START_STATE_WAIT_PROMPT2;
      } else if (self->start_state == START_STATE_WAIT_PROMPT2 && strcmp(line->str, "*") == 0) {
        self->start_state = START_STATE_DONE;
        g_cond_broadcast(&self->cond);
      }
      g_string_free(line, TRUE);
      continue;
    }
    GlideProcessMessageCallback cb = self->msg_cb;
    gpointer cb_data = self->msg_cb_data;
    g_mutex_unlock(&self->mutex);
    if (cb)
      cb(line, cb_data);
    g_string_free(line, TRUE);
    g_mutex_lock(&self->mutex);
  }
  g_mutex_unlock(&self->mutex);
}

static void on_proc_err(GString *data, gpointer user_data) {
  on_proc_out(data, user_data);
}

static void gp_start(GlideProcess *base) {
  g_debug("RealGlideProcess.start");
  RealGlideProcess *self = (RealGlideProcess*)base;
  if (self->started || !self->proc)
    return;
  g_mutex_lock(&self->mutex);
  self->start_state = START_STATE_WAIT_PROMPT;
  g_mutex_unlock(&self->mutex);
  process_start(self->proc);
  g_mutex_lock(&self->mutex);
  while (self->start_state == START_STATE_WAIT_PROMPT)
    g_cond_wait(&self->cond, &self->mutex);
  g_mutex_unlock(&self->mutex);
  const char *req = "(require :glide)\n";
  process_write(self->proc, req, -1);
  g_mutex_lock(&self->mutex);
  while (self->start_state != START_STATE_DONE)
    g_cond_wait(&self->cond, &self->mutex);
  g_mutex_unlock(&self->mutex);
  const char *start = "(glide:start-server)\n";
  process_write(self->proc, start, -1);
  g_mutex_lock(&self->mutex);
  self->started = TRUE;
  g_mutex_unlock(&self->mutex);
}

static void gp_send(GlideProcess *base, const GString *payload) {
  RealGlideProcess *self = (RealGlideProcess*)base;
  process_write(self->proc, payload->str, payload->len);
}

static void gp_set_message_cb(GlideProcess *base, GlideProcessMessageCallback cb,
                              gpointer user_data) {
  RealGlideProcess *self = (RealGlideProcess*)base;
  self->msg_cb = cb;
  self->msg_cb_data = user_data;
}

static void gp_destroy(GlideProcess *base) {
  g_debug("RealGlideProcess.destroy");
  RealGlideProcess *self = (RealGlideProcess*)base;
  if (self->proc)
    process_unref(self->proc);
  g_string_free(self->buffer, TRUE);
  g_mutex_clear(&self->mutex);
  g_cond_clear(&self->cond);
  g_free(self);
}

GlideProcess *real_glide_process_new(Process *proc) {
  g_debug("RealGlideProcess.new");
  RealGlideProcess *self = g_new0(RealGlideProcess, 1);
  self->base.ops = &real_glide_process_ops;
  self->base.refcnt = 1;
  self->proc = proc ? process_ref(proc) : NULL;
  self->buffer = g_string_new(NULL);
  g_mutex_init(&self->mutex);
  g_cond_init(&self->cond);
  self->msg_cb = NULL;
  self->msg_cb_data = NULL;
  self->started = FALSE;
  self->start_state = START_STATE_IDLE;
  if (proc) {
    process_set_stdout_cb(proc, on_proc_out, self);
    process_set_stderr_cb(proc, on_proc_err, self);
  }
  return (GlideProcess*)self;
}

