#include "real_swank_process.h"

#include "syscalls.h"
#include "util.h"

#include <gio/gio.h>
#include <unistd.h>
#include <string.h>
#include <errno.h>

struct _RealSwankProcess {
  GObject parent_instance;
  Process *proc;
  Preferences *prefs;
  int swank_fd;
  GSocketConnection *connection;
  GString *out_data;
  gsize out_consumed;
  GMutex out_mutex;
  GCond  out_cond;
  GString *swank_data;
  gsize swank_consumed;
  GMutex swank_mutex;
  SwankProcessMessageCallback msg_cb;
  gpointer msg_cb_data;
  int port;
  GThread *swank_thread;
  gboolean started;
};

static void sp_start(SwankProcess *self);
static void sp_send(SwankProcess *self, const GString *payload);
static void sp_set_message_cb(SwankProcess *self, SwankProcessMessageCallback cb,
                              gpointer user_data);

static void
real_swank_process_swank_process_iface_init(SwankProcessInterface *iface)
{
  g_debug("RealSwankProcess.swank_process_iface_init");
  iface->start = sp_start;
  iface->send = sp_send;
  iface->set_message_cb = sp_set_message_cb;
}

G_DEFINE_TYPE_WITH_CODE(RealSwankProcess, real_swank_process, G_TYPE_OBJECT,
    G_IMPLEMENT_INTERFACE(SWANK_PROCESS_TYPE, real_swank_process_swank_process_iface_init))

static void
real_swank_process_finalize(GObject *obj)
{
  g_debug("RealSwankProcess.finalize");
  RealSwankProcess *self = GLIDE_REAL_SWANK_PROCESS(obj);
  if (self->swank_fd >= 0)
    close(self->swank_fd);
  if (self->swank_thread)
    g_thread_join(self->swank_thread);
  if (self->proc)
    g_object_unref(self->proc);
  if (self->prefs)
    g_object_unref(self->prefs);
  if (self->connection)
    g_object_unref(self->connection);
  g_string_free(self->out_data, TRUE);
  g_string_free(self->swank_data, TRUE);
  g_mutex_clear(&self->out_mutex);
  g_cond_clear(&self->out_cond);
  g_mutex_clear(&self->swank_mutex);
  G_OBJECT_CLASS(real_swank_process_parent_class)->finalize(obj);
}

static void
real_swank_process_class_init(RealSwankProcessClass *klass)
{
  g_debug("RealSwankProcess.class_init");
  GObjectClass *obj = G_OBJECT_CLASS(klass);
  obj->finalize = real_swank_process_finalize;
}

static void
real_swank_process_init(RealSwankProcess *self)
{
  g_debug("RealSwankProcess.init");
  self->proc = NULL;
  self->prefs = NULL;
  self->swank_fd = -1;
  self->connection = NULL;
  self->out_data = g_string_new(NULL);
  self->swank_data = g_string_new(NULL);
  g_mutex_init(&self->out_mutex);
  g_cond_init(&self->out_cond);
  g_mutex_init(&self->swank_mutex);
  self->port = 4005;
  self->swank_thread = NULL;
  self->msg_cb = NULL;
  self->msg_cb_data = NULL;
  self->started = FALSE;
}

static gpointer
swank_reader_thread(gpointer data)
{
  g_debug("RealSwankProcess.swank_reader_thread");
  RealSwankProcess *self = data;
  char buf[1024];
  for (;;) {
    ssize_t n = sys_read(self->swank_fd, buf, sizeof(buf));
    if (n > 0) {
      char *dbg = g_strndup(buf, n);
      g_debug_40("RealSwankProcess.swank_reader_thread got:", dbg);
      g_free(dbg);
      g_mutex_lock(&self->swank_mutex);
      g_string_append_len(self->swank_data, buf, n);
      if (self->msg_cb) {
        while (TRUE) {
          if (self->swank_data->len - self->swank_consumed >= 6) {
            char hdr[7];
            memcpy(hdr, self->swank_data->str + self->swank_consumed, 6);
            hdr[6] = '\0';
            gsize len = g_ascii_strtoll(hdr, NULL, 16);
            if (self->swank_data->len - self->swank_consumed - 6 >= len) {
              char *start = self->swank_data->str + self->swank_consumed + 6;
              GString *msg = g_string_new_len(start, len);
              self->swank_consumed += 6 + len;
              self->msg_cb(msg, self->msg_cb_data);
              g_string_free(msg, TRUE);
              continue;
            }
          }
          break;
        }
      }
      g_mutex_unlock(&self->swank_mutex);
    } else if (n == -1 && (errno == EAGAIN || errno == EWOULDBLOCK)) {
      g_usleep(10000);
    } else {
      if (n == -1)
        g_printerr("swank_reader_thread read error: %s (errno %d)\n", g_strerror(errno), errno);
      else
        g_debug("RealSwankProcess.swank_reader_thread eof");
      break;
    }
  }
  return NULL;
}

static void
read_until(RealSwankProcess *self, const char *pattern)
{
  g_debug("RealSwankProcess.read_until %s", pattern);
  const gsize patlen = strlen(pattern);
  g_mutex_lock(&self->out_mutex);
  while (TRUE) {
    const char *start = self->out_data->str + self->out_consumed;
    const char *hit = strstr(start, pattern);
    if (hit) {
      gsize end = (hit - self->out_data->str) + patlen;
      self->out_consumed = end;
      g_mutex_unlock(&self->out_mutex);
      return;
    }
    g_cond_wait(&self->out_cond, &self->out_mutex);
  }
}

static void
on_proc_out(GString *data, gpointer user_data)
{
  g_debug("RealSwankProcess.on_proc_out %s", data->str);
  RealSwankProcess *self = user_data;
  g_mutex_lock(&self->out_mutex);
  g_string_append_len(self->out_data, data->str, data->len);
  g_cond_broadcast(&self->out_cond);
  g_mutex_unlock(&self->out_mutex);
}

static void
on_proc_err(GString *data, gpointer /*user_data*/)
{
  g_debug("RealSwankProcess.on_proc_err %s", data->str);
}

static void start_swank(RealSwankProcess *self)
{
  g_debug("RealSwankProcess.start_swank");
  if (!self->proc)
    return;
  read_until(self, "* ");
  // TODO: select the right loading command: (ql:quickload :swank) or (require :swank)
  const char *ql_cmd = "(require :swank)\n";
  g_debug("RealSwankProcess.start_swank send:%s", ql_cmd);
  process_write(self->proc, ql_cmd, -1);
  // TODO: select the right output:
  // - (:SWANK) for quickload
  // - ("SB-INTROSPECT" "SB-CLTL2") for require
  read_until(self, "(\"SB-INTROSPECT\" \"SB-CLTL2\")");
  read_until(self, "* ");
  char create_cmd[128];
  g_snprintf(create_cmd, sizeof(create_cmd),
      "(swank:create-server :port %d :dont-close t)\n", self->port);
  g_debug("RealSwankProcess.start_swank send:%s", create_cmd);
  process_write(self->proc, create_cmd, -1);
  read_until(self, "* ");
}

static void connect_swank(RealSwankProcess *self)
{
  g_debug("RealSwankProcess.connect_swank port:%d", self->port);
  GSocketClient *client = g_socket_client_new();
  GError *conn_err = NULL;
  GSocketConnection *conn = g_socket_client_connect_to_host(
      client, "127.0.0.1", self->port, NULL, &conn_err);
  g_object_unref(client);
  if (!conn) {
    g_clear_error(&conn_err);
    return;
  }
  self->swank_fd = g_socket_get_fd(g_socket_connection_get_socket(conn));
  self->connection = conn;
  g_debug("RealSwankProcess.connect_swank connected fd:%d", self->swank_fd);
  self->swank_thread = g_thread_new("swank-reader", swank_reader_thread, self);
}

static void
sp_start(SwankProcess *base)
{
  g_debug("RealSwankProcess.start");
  RealSwankProcess *self = GLIDE_REAL_SWANK_PROCESS(base);
  if (self->started)
    return;
  if (!self->proc || !self->prefs)
    return;
  process_start(self->proc);
  start_swank(self);
  connect_swank(self);
  self->started = TRUE;
}

SwankProcess *
real_swank_process_new(Process *proc, Preferences *prefs)
{
  g_debug("RealSwankProcess.new");
  RealSwankProcess *self = g_object_new(REAL_SWANK_PROCESS_TYPE, NULL);
  self->proc = proc ? g_object_ref(proc) : NULL;
  self->prefs = prefs ? g_object_ref(prefs) : NULL;
  self->port = prefs ? preferences_get_swank_port(prefs) : 4005;

  if (proc) {
    process_set_stdout_cb(proc, on_proc_out, self);
    process_set_stderr_cb(proc, on_proc_err, self);
  }
  return GLIDE_SWANK_PROCESS(self);
}

void
real_swank_process_set_socket(RealSwankProcess *self, int fd)
{
  g_debug("RealSwankProcess.set_socket %d", fd);
  if (self->swank_fd >= 0)
    close(self->swank_fd);
  self->swank_fd = fd;
}

static void
sp_send(SwankProcess *base, const GString *payload)
{
  RealSwankProcess *self = GLIDE_REAL_SWANK_PROCESS(base);
  size_t len = payload->len;
  char hdr[7];
  g_snprintf(hdr, sizeof(hdr), "%06zx", len);
  g_debug("RealSwankProcess.send %s%.*s", hdr, (int)len, payload->str);
  ssize_t nw = sys_write(self->swank_fd, hdr, 6);
  if (nw != 6)
    g_printerr("Failed to write swank header (errno %d)\n", errno);
  nw = sys_write(self->swank_fd, payload->str, len);
  if (nw != (ssize_t)len)
    g_printerr("Failed to write swank payload (errno %d)\n", errno);
}

static void
sp_set_message_cb(SwankProcess *base, SwankProcessMessageCallback cb,
                  gpointer user_data)
{
  g_debug("RealSwankProcess.set_message_cb");
  RealSwankProcess *self = GLIDE_REAL_SWANK_PROCESS(base);
  g_mutex_lock(&self->swank_mutex);
  self->msg_cb = cb;
  self->msg_cb_data = user_data;
  g_mutex_unlock(&self->swank_mutex);
}

