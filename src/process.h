#pragma once

#include <glib.h>

typedef struct _Process Process;

typedef void (*ProcessCallback)(GString *data, gpointer user_data);

typedef struct {
  void     (*start)(Process *self);
  void     (*set_stdout_cb)(Process *self, ProcessCallback cb, gpointer user_data);
  void     (*set_stderr_cb)(Process *self, ProcessCallback cb, gpointer user_data);
  gboolean (*write)(Process *self, const gchar *data, gssize len);
  void     (*destroy)(Process *self);
} ProcessOps;

struct _Process {
  const ProcessOps *ops;
  int refcnt;
};

static inline void process_set_stdout_cb(Process *self, ProcessCallback cb, gpointer user_data) {
  g_return_if_fail(self);
  self->ops->set_stdout_cb(self, cb, user_data);
}

static inline void process_set_stderr_cb(Process *self, ProcessCallback cb, gpointer user_data) {
  g_return_if_fail(self);
  self->ops->set_stderr_cb(self, cb, user_data);
}

static inline gboolean process_write(Process *self, const gchar *data, gssize len) {
  g_return_val_if_fail(self, FALSE);
  return self->ops->write(self, data, len);
}

static inline void process_start(Process *self) {
  g_return_if_fail(self);
  self->ops->start(self);
}

Process *process_ref(Process *self);
void     process_unref(Process *self);

