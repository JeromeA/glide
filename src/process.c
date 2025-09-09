#include "process.h"
#include "syscalls.h"

#include <unistd.h>
#include <errno.h>
#include <string.h>
#include <signal.h>
#include <sys/prctl.h>

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

static gpointer stdout_thread(gpointer data) {
  Process *process = data;
  char buf[256];
  ssize_t n;
  while ((n = sys_read(process->out_fd, buf, sizeof(buf))) > 0) {
    if (process->out_cb) {
      GString *str = g_string_new_len(buf, n);
      process->out_cb(str, process->out_user);
      g_string_free(str, TRUE);
    }
  }
  return NULL;
}

static gpointer stderr_thread(gpointer data) {
  Process *process = data;
  char buf[256];
  ssize_t n;
  while ((n = sys_read(process->err_fd, buf, sizeof(buf))) > 0) {
    if (process->err_cb) {
      GString *str = g_string_new_len(buf, n);
      process->err_cb(str, process->err_user);
      g_string_free(str, TRUE);
    }
  }
  return NULL;
}

static void child_setup(gpointer /*user_data*/) {
  setsid();
  prctl(PR_SET_PDEATHSIG, SIGKILL);
}

Process *process_ref(Process *self) {
  g_return_val_if_fail(self, NULL);
  self->refcnt++;
  return self;
}

static void process_destroy(Process *process) {
  if (process->out_thread)
    g_thread_join(process->out_thread);
  if (process->err_thread)
    g_thread_join(process->err_thread);
  if (process->in_fd >= 0) close(process->in_fd);
  if (process->out_fd >= 0) close(process->out_fd);
  if (process->err_fd >= 0) close(process->err_fd);
  if (process->pid)
    g_spawn_close_pid(process->pid);
  g_strfreev(process->argv);
  g_free(process);
}

void process_unref(Process *self) {
  if (!self)
    return;
  if (--self->refcnt == 0)
    process_destroy(self);
}

void process_set_stdout_cb(Process *process, ProcessCallback cb, gpointer user_data) {
  g_return_if_fail(process);
  process->out_cb = cb;
  process->out_user = user_data;
  if (cb && process->started && !process->out_thread)
    process->out_thread = g_thread_new("process-stdout", stdout_thread, process);
}

void process_set_stderr_cb(Process *process, ProcessCallback cb, gpointer user_data) {
  g_return_if_fail(process);
  process->err_cb = cb;
  process->err_user = user_data;
  if (cb && process->started && !process->err_thread)
    process->err_thread = g_thread_new("process-stderr", stderr_thread, process);
}

void process_start(Process *process) {
  g_return_if_fail(process);
  if (process->started || !process->argv)
    return;
  GError *error = NULL;
  if (!g_spawn_async_with_pipes(NULL, process->argv, NULL,
        G_SPAWN_DO_NOT_REAP_CHILD | G_SPAWN_SEARCH_PATH,
        child_setup, NULL,
        &process->pid,
        &process->in_fd,
        &process->out_fd,
        &process->err_fd,
        &error)) {
    g_clear_error(&error);
    return;
  }
  process->started = TRUE;
  if (!process->out_thread)
    process->out_thread = g_thread_new("process-stdout", stdout_thread, process);
  if (!process->err_thread)
    process->err_thread = g_thread_new("process-stderr", stderr_thread, process);
}

gboolean process_write(Process *process, const GString *data) {
  g_return_val_if_fail(process, FALSE);
  g_return_val_if_fail(data, FALSE);
  gssize len = data->len;
  gssize written = 0;
  while (written < len) {
    ssize_t r = sys_write(process->in_fd,
        data->str + written, len - written);
    if (r <= 0)
      return FALSE;
    written += r;
  }
  return TRUE;
}

Process *process_new_from_argv(const gchar *const *argv) {
  g_return_val_if_fail(argv, NULL);
  Process *process = g_new0(Process, 1);
  process->pid = 0;
  process->refcnt = 1;
  process->in_fd = process->out_fd = process->err_fd = -1;
  process->out_thread = NULL;
  process->err_thread = NULL;
  process->out_cb = NULL;
  process->out_user = NULL;
  process->err_cb = NULL;
  process->err_user = NULL;
  process->argv = g_strdupv((gchar**)argv);
  process->started = FALSE;
  return process;
}

Process *process_new(const gchar *cmd) {
  if (!cmd)
    return NULL;
  const gchar *argv[] = { cmd, NULL };
  return process_new_from_argv(argv);
}

