#undef G_LOG_DOMAIN
#define G_LOG_DOMAIN "process"
#include "process.h"
#include "preferences.h"
#include "syscalls.h"

#include <glib.h>
#include <gio/gio.h>
#include <unistd.h>
#include <errno.h>
#include <string.h>
#include <signal.h>
#include <sys/prctl.h>

typedef struct {
  ProcessImpl base;
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
} RealProcess;

static gpointer
stdout_thread(gpointer data)
{
  RealProcess *p = data;
  g_debug("stdout_thread started for pid %d", (int)p->pid);
  char buf[256];
  ssize_t n;
  while ((n = sys_read(p->out_fd, buf, sizeof(buf))) > 0) {
    if (p->out_cb) {
      GString *s = g_string_new_len(buf, n);
      g_debug("stdout_thread read: %.*s", (int)s->len, s->str);
      p->out_cb(s, p->out_user);
      g_string_free(s, TRUE);
    }
  }
  g_debug("stdout_thread exiting");
  return NULL;
}

static gpointer
stderr_thread(gpointer data)
{
  RealProcess *p = data;
  g_debug("stderr_thread started for pid %d", (int)p->pid);
  char buf[256];
  ssize_t n;
  while ((n = sys_read(p->err_fd, buf, sizeof(buf))) > 0) {
    if (p->err_cb) {
      GString *s = g_string_new_len(buf, n);
      g_debug("stderr_thread read: %.*s", (int)s->len, s->str);
      p->err_cb(s, p->err_user);
      g_string_free(s, TRUE);
    }
  }
  g_debug("stderr_thread exiting");
  return NULL;
}

static void
child_setup(gpointer /*user_data*/)
{
  setsid();
  prctl(PR_SET_PDEATHSIG, SIGKILL);
}

static void proc_real_set_stdout_cb(ProcessImpl *proc, ProcessCallback cb, gpointer user_data);
static void proc_real_set_stderr_cb(ProcessImpl *proc, ProcessCallback cb, gpointer user_data);
static gboolean proc_real_write(ProcessImpl *proc, const gchar *data, gssize len);
static void proc_real_free(ProcessImpl *proc);

static const Process process_real_iface = {
  proc_real_set_stdout_cb,
  proc_real_set_stderr_cb,
  proc_real_write,
  proc_real_free
};

static ProcessImpl *
process_spawn(const gchar *const *argv)
{
  RealProcess *p = g_new0(RealProcess,1);
  p->base.iface = &process_real_iface;
  g_debug("spawning: %s", argv[0]);
  GError *error = NULL;
  if (!g_spawn_async_with_pipes(NULL, (gchar**)argv, NULL,
        G_SPAWN_DO_NOT_REAP_CHILD | G_SPAWN_SEARCH_PATH,
        child_setup, NULL,
        &p->pid,
        &p->in_fd,
        &p->out_fd,
        &p->err_fd,
        &error)) {
    g_warning("process_new: %s", error->message);
    g_clear_error(&error);
    g_free(p);
    return NULL;
  }
  g_debug("spawned pid %d", (int)p->pid);
  return &p->base;
}

ProcessImpl *
process_new_from_argv(const gchar *const *argv)
{
  g_debug("process_new_from_argv: %s", argv[0]);
  return process_spawn(argv);
}

ProcessImpl *
process_new(Preferences *prefs)
{
  g_debug("process_new called");
  if (!prefs)
    return NULL;
  const gchar *sdk = preferences_get_sdk(prefs);
  if (!sdk)
    return NULL;
  const gchar *argv[] = { sdk, NULL };
  g_debug("using sdk: %s", sdk);
  return process_spawn(argv);
}

static void
proc_real_set_stdout_cb(ProcessImpl *proc, ProcessCallback cb, gpointer user_data)
{
  RealProcess *p = (RealProcess*)proc;
  g_debug("set_stdout_cb %p", cb);
  p->out_cb = cb;
  p->out_user = user_data;
  if (cb && !p->out_thread)
    p->out_thread = g_thread_new("process-stdout", stdout_thread, p);
}

static void
proc_real_set_stderr_cb(ProcessImpl *proc, ProcessCallback cb, gpointer user_data)
{
  RealProcess *p = (RealProcess*)proc;
  g_debug("set_stderr_cb %p", cb);
  p->err_cb = cb;
  p->err_user = user_data;
  if (cb && !p->err_thread)
    p->err_thread = g_thread_new("process-stderr", stderr_thread, p);
}

static gboolean
proc_real_write(ProcessImpl *proc, const gchar *data, gssize len)
{
  RealProcess *p = (RealProcess*)proc;
  if (len < 0)
    len = strlen(data);
  g_debug("write %.*s", (int)len, data);
  return sys_write(p->in_fd, data, len) == len;
}

static void
proc_real_free(ProcessImpl *proc)
{
  RealProcess *p = (RealProcess*)proc;
  if (!p) return;
  g_debug("freeing process pid %d", (int)p->pid);
  if (p->out_thread)
    g_thread_join(p->out_thread);
  if (p->err_thread)
    g_thread_join(p->err_thread);
  if (p->in_fd >= 0) { g_debug("closing in fd %d", p->in_fd); close(p->in_fd); }
  if (p->out_fd >= 0) { g_debug("closing out fd %d", p->out_fd); close(p->out_fd); }
  if (p->err_fd >= 0) { g_debug("closing err fd %d", p->err_fd); close(p->err_fd); }
  if (p->pid)
    g_spawn_close_pid(p->pid);
  g_free(p);
}

void
process_set_stdout_cb(ProcessImpl *proc, ProcessCallback cb, gpointer user_data)
{
  g_return_if_fail(proc && proc->iface && proc->iface->set_stdout_cb);
  g_debug("process_set_stdout_cb");
  proc->iface->set_stdout_cb(proc, cb, user_data);
}

void
process_set_stderr_cb(ProcessImpl *proc, ProcessCallback cb, gpointer user_data)
{
  g_return_if_fail(proc && proc->iface && proc->iface->set_stderr_cb);
  g_debug("process_set_stderr_cb");
  proc->iface->set_stderr_cb(proc, cb, user_data);
}

gboolean
process_write(ProcessImpl *proc, const gchar *data, gssize len)
{
  g_return_val_if_fail(proc && proc->iface && proc->iface->write, FALSE);
  g_debug("process_write wrapper");
  return proc->iface->write(proc, data, len);
}

void
process_free(ProcessImpl *proc)
{
  if (!proc || !proc->iface || !proc->iface->free)
    return;
  g_debug("process_free wrapper");
  proc->iface->free(proc);
}
