#include "real_process.h"
#include "syscalls.h"

#include <gio/gio.h>
#include <unistd.h>
#include <errno.h>
#include <string.h>
#include <signal.h>
#include <sys/prctl.h>

static gpointer stdout_thread(gpointer data) {
  g_debug("RealProcess.stdout_thread");
  RealProcess *p = data;
  char buf[256];
  ssize_t n;
  while ((n = sys_read(p->out_fd, buf, sizeof(buf))) > 0) {
    if (p->out_cb) {
      GString *s = g_string_new_len(buf, n);
      p->out_cb(s, p->out_user);
      g_string_free(s, TRUE);
    }
  }
  return NULL;
}

static gpointer stderr_thread(gpointer data) {
  g_debug("RealProcess.stderr_thread");
  RealProcess *p = data;
  char buf[256];
  ssize_t n;
  while ((n = sys_read(p->err_fd, buf, sizeof(buf))) > 0) {
    if (p->err_cb) {
      GString *s = g_string_new_len(buf, n);
      p->err_cb(s, p->err_user);
      g_string_free(s, TRUE);
    }
  }
  return NULL;
}

static void child_setup(gpointer /*user_data*/) {
  g_debug("RealProcess.child_setup");
  setsid();
  prctl(PR_SET_PDEATHSIG, SIGKILL);
}

static void real_set_stdout_cb(Process *proc, ProcessCallback cb, gpointer user_data) {
  g_debug("RealProcess.set_stdout_cb");
  RealProcess *p = (RealProcess*)proc;
  p->out_cb = cb;
  p->out_user = user_data;
  if (cb && p->started && !p->out_thread)
    p->out_thread = g_thread_new("process-stdout", stdout_thread, p);
}

static void real_set_stderr_cb(Process *proc, ProcessCallback cb, gpointer user_data) {
  g_debug("RealProcess.set_stderr_cb");
  RealProcess *p = (RealProcess*)proc;
  p->err_cb = cb;
  p->err_user = user_data;
  if (cb && p->started && !p->err_thread)
    p->err_thread = g_thread_new("process-stderr", stderr_thread, p);
}

static void real_start(Process *proc) {
  g_debug("RealProcess.start");
  RealProcess *p = (RealProcess*)proc;
  if (p->started || !p->argv)
    return;
  GError *error = NULL;
  if (!g_spawn_async_with_pipes(NULL, p->argv, NULL,
        G_SPAWN_DO_NOT_REAP_CHILD | G_SPAWN_SEARCH_PATH,
        child_setup, NULL,
        &p->pid,
        &p->in_fd,
        &p->out_fd,
        &p->err_fd,
        &error)) {
    g_clear_error(&error);
    return;
  }
  p->started = TRUE;
  if (!p->out_thread)
    p->out_thread = g_thread_new("process-stdout", stdout_thread, p);
  if (!p->err_thread)
    p->err_thread = g_thread_new("process-stderr", stderr_thread, p);
}

static gboolean real_write(Process *proc, const gchar *data, gssize len) {
  g_debug("RealProcess.write %zd", len);
  RealProcess *p = (RealProcess*)proc;
  if (len < 0)
    len = strlen(data);
  return sys_write(p->in_fd, data, len) == len;
}

static void real_destroy(Process *proc) {
  g_debug("RealProcess.destroy");
  RealProcess *p = (RealProcess*)proc;
  if (p->out_thread)
    g_thread_join(p->out_thread);
  if (p->err_thread)
    g_thread_join(p->err_thread);
  if (p->in_fd >= 0) close(p->in_fd);
  if (p->out_fd >= 0) close(p->out_fd);
  if (p->err_fd >= 0) close(p->err_fd);
  if (p->pid)
    g_spawn_close_pid(p->pid);
  g_strfreev(p->argv);
  g_free(p);
}

static const ProcessOps real_process_ops = {
  .start = real_start,
  .set_stdout_cb = real_set_stdout_cb,
  .set_stderr_cb = real_set_stderr_cb,
  .write = real_write,
  .destroy = real_destroy,
};

Process *real_process_new_from_argv(const gchar *const *argv) {
  g_debug("RealProcess.new_from_argv cmd:%s", argv && argv[0] ? argv[0] : "(null)");
  RealProcess *p = g_new0(RealProcess, 1);
  p->base.ops = &real_process_ops;
  p->base.refcnt = 1;
  p->pid = 0;
  p->in_fd = p->out_fd = p->err_fd = -1;
  p->out_thread = NULL;
  p->err_thread = NULL;
  p->argv = g_strdupv((gchar**)argv);
  p->started = FALSE;
  return (Process*)p;
}

Process *real_process_new(const gchar *cmd) {
  g_debug("RealProcess.new cmd:%s", cmd ? cmd : "(null)");
  if (!cmd)
    return NULL;
  const gchar *argv[] = { cmd, NULL };
  return real_process_new_from_argv(argv);
}
