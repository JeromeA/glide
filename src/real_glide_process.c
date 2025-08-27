#include "real_glide_process.h"
#include "util.h"

#include <string.h>

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
  process_start(self->proc);
  const char *init = "(require :glide)\n(glide:start-server)\n";
  process_write(self->proc, init, -1);
  self->started = TRUE;
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
  self->msg_cb = NULL;
  self->msg_cb_data = NULL;
  self->started = FALSE;
  if (proc) {
    process_set_stdout_cb(proc, on_proc_out, self);
    process_set_stderr_cb(proc, on_proc_err, self);
  }
  return (GlideProcess*)self;
}

