#pragma once

#include <glib.h>

typedef struct _Process Process;

typedef void (*ProcessCallback)(GString *data, gpointer user_data);

Process *process_new(const gchar *cmd);
Process *process_new_from_argv(const gchar *const *argv);
void     process_set_stdout_cb(Process *self, ProcessCallback cb, gpointer user_data);
void     process_set_stderr_cb(Process *self, ProcessCallback cb, gpointer user_data);
void     process_start(Process *self);
gboolean process_write(Process *self, const GString *data);
Process *process_ref(Process *self);
void     process_unref(Process *self);
