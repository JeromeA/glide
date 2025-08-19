#ifndef REAL_PROCESS_H
#define REAL_PROCESS_H

#include "process.h"

typedef struct {
  Process base;
  GPid pid;
  int in_fd;
  int out_fd;
  int err_fd;
  ProcessCallback out_cb;
  gpointer out_user;
  ProcessCallback err_cb;
  gpointer err_user;
  GThread *out_thread;
  GThread *err_thread;
  gchar **argv;
  gboolean started;
} RealProcess;

Process *real_process_new(const gchar *cmd);
Process *real_process_new_from_argv(const gchar *const *argv);

#endif /* REAL_PROCESS_H */
