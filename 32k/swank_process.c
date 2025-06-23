#undef G_LOG_DOMAIN
#define G_LOG_DOMAIN "swank-process"
#include "swank_process.h"
#include "syscalls.h"

#include <gio/gio.h>
#include <unistd.h>
#include <string.h>
#include <errno.h>

typedef struct {
  SwankProcessImpl base;
  ProcessImpl *proc;
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
  GCond  swank_cond;

  int port;
  GThread *swank_thread;
} RealSwankProcess;

static void sp_real_send(SwankProcessImpl *self, const GString *payload);
static GString *sp_real_get_reply(SwankProcessImpl *self);
static void sp_real_free(SwankProcessImpl *self);

static const SwankProcess swank_real_iface = {
  sp_real_send,
  sp_real_get_reply,
  sp_real_free
};

static void
on_proc_out(GString *data, gpointer user_data)
{
  RealSwankProcess *self = user_data;
  g_debug("stdout: %.*s", (int)data->len, data->str);
  g_mutex_lock(&self->out_mutex);
  g_string_append_len(self->out_data, data->str, data->len);
  g_cond_broadcast(&self->out_cond);
  g_mutex_unlock(&self->out_mutex);
}

static void
on_proc_err(GString *data, gpointer /*user_data*/)
{
  /* just print */
  g_debug("stderr: %.*s", (int)data->len, data->str);
  g_printerr("%.*s", (int)data->len, data->str);
}

static gpointer
swank_reader_thread(gpointer data)
{
  RealSwankProcess *self = data;
  g_debug("swank_reader_thread started");
  char buf[1024];
  for(;;) {
    ssize_t n = sys_read(self->swank_fd, buf, sizeof(buf));
    if (n > 0) {
      g_debug("swank read %zd bytes", n);
      g_mutex_lock(&self->swank_mutex);
      g_string_append_len(self->swank_data, buf, n);
      g_cond_broadcast(&self->swank_cond);
      g_mutex_unlock(&self->swank_mutex);
    } else if (n == -1 && (errno == EAGAIN || errno == EWOULDBLOCK)) {
      g_usleep(10000);
    } else {
      break;
    }
  }
  g_debug("swank_reader_thread exiting");
  return NULL;
}

static void
read_until(RealSwankProcess *self, const char *pattern)
{
  const gsize patlen = strlen(pattern);
  g_debug("read_until: waiting for '%s'", pattern);
  g_mutex_lock(&self->out_mutex);
  while (TRUE) {
    const char *start = self->out_data->str + self->out_consumed;
    const char *hit = strstr(start, pattern);
    if (hit) {
      gsize end = (hit - self->out_data->str) + patlen;
      self->out_consumed = end;
      g_mutex_unlock(&self->out_mutex);
      g_debug("read_until: found '%s'", pattern);
      return;
    }
    g_cond_wait(&self->out_cond, &self->out_mutex);
  }
}


static void start_swank(RealSwankProcess *self)
{
  if (!self->proc)
    return;
  g_debug("starting swank on port %d", self->port);
  read_until(self, "* ");
  const char *ql_cmd = "(ql:quickload :swank)\n";
  process_write(self->proc, ql_cmd, -1);
  read_until(self, "(:SWANK)");
  read_until(self, "* ");
  char create_cmd[128];
  g_snprintf(create_cmd, sizeof(create_cmd),
      "(swank:create-server :port %d :dont-close t)\n", self->port);
  process_write(self->proc, create_cmd, -1);
  read_until(self, "* ");
  g_debug("swank started");
}

static void connect_swank(RealSwankProcess *self)
{
  GSocketClient *client = g_socket_client_new();
  GError *conn_err = NULL;
  g_debug("connecting to swank on port %d", self->port);
  GSocketConnection *conn = g_socket_client_connect_to_host(
      client, "127.0.0.1", self->port, NULL, &conn_err);
  g_object_unref(client);
  if (!conn) {
    g_clear_error(&conn_err);
    g_debug("failed to connect to swank");
    return;
  }
  self->swank_fd = g_socket_get_fd(g_socket_connection_get_socket(conn));
  self->connection = conn; /* hold for lifetime */
  self->swank_thread = g_thread_new("swank-reader", swank_reader_thread, self);
  g_debug("connected to swank");
}

SwankProcessImpl *
swank_process_new(ProcessImpl *proc, Preferences *prefs)
{
  RealSwankProcess *self = g_new0(RealSwankProcess,1);
  g_debug("swank_process_new");
  self->base.iface = &swank_real_iface;
  self->prefs = prefs;
  self->proc = proc;
  self->out_data = g_string_new(NULL);
  self->swank_data = g_string_new(NULL);
  g_mutex_init(&self->out_mutex);
  g_cond_init(&self->out_cond);
  g_mutex_init(&self->swank_mutex);
  g_cond_init(&self->swank_cond);
  self->connection = NULL;
  self->port = prefs ? preferences_get_swank_port(prefs) : 4005;

  if (proc) {
    process_set_stdout_cb(proc, on_proc_out, self);
    process_set_stderr_cb(proc, on_proc_err, self);
  }
  if (prefs && proc) {
    start_swank(self);
    connect_swank(self);
  }
  return &self->base;
}

static void
sp_real_send(SwankProcessImpl *base, const GString *payload)
{
  RealSwankProcess *self = (RealSwankProcess*)base;
  size_t len = payload->len;
  char hdr[7];
  g_snprintf(hdr, sizeof(hdr), "%06zx", len);
  g_debug("sending header: %.*s", 6, hdr);
  sys_write(self->swank_fd, hdr, 6);
  g_debug("sending payload: %.*s", (int)len, payload->str);
  sys_write(self->swank_fd, payload->str, len);
}

static GString *
extract_reply(RealSwankProcess *self)
{
  for(;;) {
    if (self->swank_data->len - self->swank_consumed >= 6) {
      char hdr[7];
      memcpy(hdr, self->swank_data->str + self->swank_consumed, 6);
      hdr[6] = '\0';
      gsize len = g_ascii_strtoll(hdr, NULL, 16);
      if (self->swank_data->len - self->swank_consumed - 6 >= len) {
        char *start = self->swank_data->str + self->swank_consumed + 6;
        GString *msg = g_string_new_len(start, len);
        self->swank_consumed += 6 + len;
        g_debug("got reply %.*s", (int)len, start);
        return msg;
      }
    }
    g_cond_wait(&self->swank_cond, &self->swank_mutex);
  }
}

static GString *
sp_real_get_reply(SwankProcessImpl *base)
{
  RealSwankProcess *self = (RealSwankProcess*)base;
  g_debug("waiting for reply");
  g_mutex_lock(&self->swank_mutex);
  GString *ret = extract_reply(self);
  g_mutex_unlock(&self->swank_mutex);
  return ret;
}

static void
sp_real_free(SwankProcessImpl *base)
{
  RealSwankProcess *self = (RealSwankProcess*)base;
  if (!self) return;
  g_debug("freeing swank process");
  if (self->swank_fd >= 0)
    { g_debug("closing fd %d", self->swank_fd); close(self->swank_fd); }
  if (self->swank_thread)
    { g_debug("joining swank thread"); g_thread_join(self->swank_thread); }
  if (self->proc)
    { g_debug("freeing child process"); process_free(self->proc); }
  if (self->connection)
    { g_debug("unreffing connection"); g_object_unref(self->connection); }
  g_string_free(self->out_data, TRUE);
  g_string_free(self->swank_data, TRUE);
  g_mutex_clear(&self->out_mutex);
  g_cond_clear(&self->out_cond);
  g_mutex_clear(&self->swank_mutex);
  g_cond_clear(&self->swank_cond);
  g_free(self);
}

void
swank_process_send(SwankProcessImpl *self, const GString *payload)
{
  g_return_if_fail(self && self->iface && self->iface->send);
  g_debug("swank_process_send %.*s", (int)payload->len, payload->str);
  self->iface->send(self, payload);
}

GString *
swank_process_get_reply(SwankProcessImpl *self)
{
  g_return_val_if_fail(self && self->iface && self->iface->get_reply, NULL);
  g_debug("swank_process_get_reply");
  return self->iface->get_reply(self);
}

void
swank_process_free(SwankProcessImpl *self)
{
  if (!self || !self->iface || !self->iface->free)
    return;
  g_debug("swank_process_free wrapper");
  self->iface->free(self);
}
