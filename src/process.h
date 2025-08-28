#pragma once

#include <glib.h>
#include <gio/gio.h>

typedef struct _Process Process;

typedef void (*ProcessCallback)(GString *data, gpointer user_data);

struct _Process {
  GPid pid;
  int refcnt;
  int in_fd;
  int out_fd;
  int err_fd;
  GThread *out_thread;
  GThread *err_thread;
  ProcessCallback out_cb;
  gpointer out_user;
  ProcessCallback err_cb;
  gpointer err_user;
  gchar **argv;
  gboolean started;
};

Process *process_new(const gchar *cmd);
Process *process_new_from_argv(const gchar *const *argv);
void     process_set_stdout_cb(Process *self, ProcessCallback cb, gpointer user_data);
void     process_set_stderr_cb(Process *self, ProcessCallback cb, gpointer user_data);
void     process_start(Process *self);
gboolean process_write(Process *self, const gchar *data, gssize len);
Process *process_ref(Process *self);
void     process_unref(Process *self);
