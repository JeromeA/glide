#include "repl_process.h"
#include "util.h"

#include <string.h>

struct _ReplProcess {
  int refcnt;
  Process *proc;
  GString *buffer;
  GMutex mutex;
  GCond cond;
  ReplProcessMessageCallback msg_cb;
  gpointer msg_cb_data;
  gboolean started;
  int start_state;
};

enum {
  START_STATE_IDLE = 0,
  START_STATE_WAIT_PROMPT,
  START_STATE_WAIT_NIL,
  START_STATE_WAIT_PROMPT2,
  START_STATE_DONE,
};

static gboolean consume_startup_line(ReplProcess *self, const gchar *line) {
  gchar *trimmed = g_strstrip(g_strdup(line));
  gboolean consumed = FALSE;
  if (self->start_state == START_STATE_WAIT_PROMPT && strcmp(trimmed, "*") == 0) {
    self->start_state = START_STATE_WAIT_NIL;
    g_cond_broadcast(&self->cond);
    consumed = TRUE;
  } else if (self->start_state == START_STATE_WAIT_NIL && strcmp(trimmed, "NIL") == 0) {
    self->start_state = START_STATE_WAIT_PROMPT2;
    consumed = TRUE;
  } else if (self->start_state == START_STATE_WAIT_PROMPT2 && strcmp(trimmed, "*") == 0) {
    self->start_state = START_STATE_DONE;
    g_cond_broadcast(&self->cond);
    consumed = TRUE;
  }
  g_free(trimmed);
  return consumed;
}

static void on_proc_out(GString *data, gpointer user_data) {
  ReplProcess *self = user_data;
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
      consume_startup_line(self, line->str);
      g_string_free(line, TRUE);
      continue;
    }
    ReplProcessMessageCallback cb = self->msg_cb;
    gpointer cb_data = self->msg_cb_data;
    g_mutex_unlock(&self->mutex);
    if (cb)
      cb(line, cb_data);
    g_string_free(line, TRUE);
    g_mutex_lock(&self->mutex);
  }
  if (self->start_state != START_STATE_DONE && self->buffer->len > 0 &&
      consume_startup_line(self, self->buffer->str)) {
    g_string_truncate(self->buffer, 0);
  }
  g_mutex_unlock(&self->mutex);
}

static void on_proc_err(GString *data, gpointer user_data) {
  on_proc_out(data, user_data);
}

ReplProcess *repl_process_ref(ReplProcess *self) {
  g_return_val_if_fail(self, NULL);
  self->refcnt++;
  return self;
}

static void repl_process_destroy(ReplProcess *self) {
  if (self->proc)
    process_unref(self->proc);
  g_string_free(self->buffer, TRUE);
  g_mutex_clear(&self->mutex);
  g_cond_clear(&self->cond);
  g_free(self);
}

void repl_process_unref(ReplProcess *self) {
  if (!self)
    return;
  if (--self->refcnt == 0)
    repl_process_destroy(self);
}

ReplProcess *repl_process_new(Process *proc) {
  ReplProcess *self = g_new0(ReplProcess, 1);
  self->refcnt = 1;
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
  return self;
}

void repl_process_start(ReplProcess *self) {
  g_return_if_fail(self);
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
  process_write(self->proc, "(require :glide)\n", -1);
  g_mutex_lock(&self->mutex);
  while (self->start_state != START_STATE_DONE)
    g_cond_wait(&self->cond, &self->mutex);
  g_mutex_unlock(&self->mutex);
  process_write(self->proc, "(glide:start-server)\n", -1);
  g_mutex_lock(&self->mutex);
  self->started = TRUE;
  g_mutex_unlock(&self->mutex);
}

void repl_process_send(ReplProcess *self, const GString *payload) {
  g_return_if_fail(self);
  process_write(self->proc, payload->str, payload->len);
}

void repl_process_set_message_cb(ReplProcess *self, ReplProcessMessageCallback cb,
                                  gpointer user_data) {
  g_return_if_fail(self);
  self->msg_cb = cb;
  self->msg_cb_data = user_data;
}

