#include "real_process.h"
#include "syscalls.h"

#include <gio/gio.h>
#include <unistd.h>
#include <errno.h>
#include <string.h>
#include <signal.h>
#include <sys/prctl.h>

struct _RealProcess {
  GObject parent_instance;
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
};

static gpointer
stdout_thread(gpointer data)
{
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

static gpointer
stderr_thread(gpointer data)
{
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

static void
child_setup(gpointer /*user_data*/)
{
  g_debug("RealProcess.child_setup");
  setsid();
  prctl(PR_SET_PDEATHSIG, SIGKILL);
}

static void real_set_stdout_cb(Process *proc, ProcessCallback cb, gpointer user_data);
static void real_set_stderr_cb(Process *proc, ProcessCallback cb, gpointer user_data);
static gboolean real_write(Process *proc, const gchar *data, gssize len);
static void real_start(Process *proc);

static void
real_process_process_iface_init(ProcessInterface *iface)
{
  g_debug("RealProcess.process_iface_init");
  iface->set_stdout_cb = real_set_stdout_cb;
  iface->set_stderr_cb = real_set_stderr_cb;
  iface->write = real_write;
  iface->start = real_start;
}

G_DEFINE_TYPE_WITH_CODE(RealProcess, real_process, G_TYPE_OBJECT,
    G_IMPLEMENT_INTERFACE(PROCESS_TYPE, real_process_process_iface_init))

static void
real_process_finalize(GObject *obj)
{
  g_debug("RealProcess.finalize");
  RealProcess *p = GLIDE_REAL_PROCESS(obj);
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
  G_OBJECT_CLASS(real_process_parent_class)->finalize(obj);
}

static void
real_process_class_init(RealProcessClass *klass)
{
  g_debug("RealProcess.class_init");
  GObjectClass *obj = G_OBJECT_CLASS(klass);
  obj->finalize = real_process_finalize;
}

static void
real_process_init(RealProcess *self)
{
  g_debug("RealProcess.init");
  self->pid = 0;
  self->in_fd = self->out_fd = self->err_fd = -1;
  self->out_cb = NULL;
  self->err_cb = NULL;
  self->out_thread = NULL;
  self->err_thread = NULL;
  self->argv = NULL;
  self->started = FALSE;
}


Process *
real_process_new_from_argv(const gchar *const *argv)
{
  g_debug("RealProcess.new_from_argv cmd:%s", argv && argv[0] ? argv[0] : "(null)");
  RealProcess *p = g_object_new(REAL_PROCESS_TYPE, NULL);
  p->argv = g_strdupv((gchar**)argv);
  return GLIDE_PROCESS(p);
}

Process *
real_process_new(const gchar *cmd)
{
  g_debug("RealProcess.new cmd:%s", cmd ? cmd : "(null)");
  if (!cmd)
    return NULL;
  const gchar *argv[] = { cmd, NULL };
  return real_process_new_from_argv(argv);
}

static void
real_set_stdout_cb(Process *proc, ProcessCallback cb, gpointer user_data)
{
  g_debug("RealProcess.set_stdout_cb");
  RealProcess *p = GLIDE_REAL_PROCESS(proc);
  p->out_cb = cb;
  p->out_user = user_data;
  if (cb && p->started && !p->out_thread)
    p->out_thread = g_thread_new("process-stdout", stdout_thread, p);
}

static void
real_set_stderr_cb(Process *proc, ProcessCallback cb, gpointer user_data)
{
  g_debug("RealProcess.set_stderr_cb");
  RealProcess *p = GLIDE_REAL_PROCESS(proc);
  p->err_cb = cb;
  p->err_user = user_data;
  if (cb && p->started && !p->err_thread)
    p->err_thread = g_thread_new("process-stderr", stderr_thread, p);
}

static void
real_start(Process *proc)
{
  g_debug("RealProcess.start");
  RealProcess *p = GLIDE_REAL_PROCESS(proc);
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

static gboolean
real_write(Process *proc, const gchar *data, gssize len)
{
  g_debug("RealProcess.write %zd", len);
  RealProcess *p = GLIDE_REAL_PROCESS(proc);
  if (len < 0)
    len = strlen(data);
  return sys_write(p->in_fd, data, len) == len;
}
