#include "swank.h"
#include "preferences.h"

#include <gio/gio.h>
#include <glib.h>
#include <unistd.h>
#include <string.h>
#include <errno.h>
#include <stdlib.h>
#include <stdio.h>
#include <sys/prctl.h>

#define DEFAULT_SWANK_PORT 4005

struct _Swank {
  GObject             parent_instance;
  GPid                lisp_pid;
  int                 in_fd;
  int                 out_fd;
  int                 err_fd;
  GSocketConnection  *connection;
  int                 swank_fd;
  int                 port;

  GString            *out_data; /* complete output so far */
  gsize               out_consumed;
  GMutex              out_mutex;
  GCond               out_cond;
  GString            *swank_data; /* complete output so far */
  gsize               swank_consumed;
  GMutex              swank_mutex;
  GCond               swank_cond;
};

G_DEFINE_TYPE(Swank, swank, G_TYPE_OBJECT)

static Swank *swank_instance = NULL;

static gpointer
stderr_reader_thread(gpointer data)
{
  int fd = GPOINTER_TO_INT(data);
  char buf[1024];
  ssize_t n;

  while ((n = read(fd, buf, sizeof(buf) - 1)) > 0) {
    buf[n] = '\0';
    g_print("err: %s\n", buf);
  }
  g_print("stderr_reader_thread: EOF\n");
  return NULL;
}

static gpointer
stdout_reader_thread (gpointer data)
{
  Swank *self = data;
  int    fd   = self->out_fd;
  char   buf[1024];
  ssize_t n;

  while ((n = read (fd, buf, sizeof(buf)-1)) > 0) {
    buf[n] = '\0';
    g_print("out: %s\n", buf);

    g_mutex_lock (&self->out_mutex);
    g_string_append_len (self->out_data, buf, n);
    g_cond_broadcast (&self->out_cond);   /* wake waiters      */
    g_mutex_unlock (&self->out_mutex);
  }
  g_print("stdout_reader_thread: EOF\n");
  return NULL;
}

static gpointer
swank_reader_thread (gpointer data)
{
  Swank *self = data;
  int    fd   = self->swank_fd;
  char   buf[1024];

  for(;;) {
    ssize_t n = read (fd, buf, sizeof(buf)-1);
    if (n > 0) {
      buf[n] = '\0';
      g_print("swank: %s\n", buf);

      g_mutex_lock (&self->swank_mutex);
      g_string_append_len (self->swank_data, buf, n);
      g_cond_broadcast (&self->swank_cond);   /* wake waiters      */
      g_mutex_unlock (&self->swank_mutex);
    } else if (n == -1 && (errno == EAGAIN || errno == EWOULDBLOCK)) {
      g_usleep (10000);           /* no data yet – wait a bit */
      continue;
    } else {
      break; /* EOF or error */
    }
  }
  g_print("swank_reader_thread: EOF\n");
  return NULL;
}

/* Wait until <pattern> occurs in self->out_data *after* self->log_pos.
 * While waiting we release the mutex and sleep on the condition.
 * When the pattern is found we print what we consumed and move
 * log_pos forward so the caller does not see the same bytes twice.
 */
static void
read_until (Swank *self, const char *pattern)
{
  g_print("  read_until: %s\n", pattern);
  const gsize patlen = strlen (pattern);

  g_mutex_lock (&self->out_mutex);
  while (TRUE) {
    const char *start = self->out_data->str + self->out_consumed;
    const char *hit   = strstr (start, pattern);

    if (hit) {
      gsize end = (hit - self->out_data->str) + patlen;

      /* optional: echo what was consumed */
      g_print ("  read_until found: %.*s\n", (int)(end - self->out_consumed), start);

      self->out_consumed = end;
      g_mutex_unlock (&self->out_mutex);
      return;
    }

    /* pattern not yet there – wait for more data                */
    g_cond_wait (&self->out_cond, &self->out_mutex);
  }
}

/* Escape a C string for inclusion in a Lisp string literal */
static gchar*
escape_string(const char *str)
{
  GString *out = g_string_new(NULL);
  for (const char *p = str; *p; p++) {
    switch (*p) {
      case '\\': g_string_append(out, "\\\\"); break;
      case '"':  g_string_append(out, "\\\""); break;
      default:
                 g_string_append_c(out, *p);
    }
  }
  return g_string_free(out, FALSE);
}

static void child_setup (gpointer)
{
  /* 1. detach from the controlling terminal */
  setsid();

  /* 2. arrange to die when our real parent dies */
  prctl (PR_SET_PDEATHSIG, SIGTERM);

  /* 3. cope with the fork–prctl race (parent might already be gone) */
  if (getppid () == 1)
    kill (getpid (), SIGTERM);
}


static void start_lisp(Swank *self)
{
  Preferences *prefs = preferences_get_instance();
  const gchar *sdk_path = preferences_get_sdk(prefs);
  if (!sdk_path) {
    g_print("Swank: SDK not configured\n");
    return;
  }

  int port = preferences_get_swank_port(prefs);
  if (port <= 0)
    port = DEFAULT_SWANK_PORT;
  self->port = port;

  GError *error = NULL;
  gint in_fd, out_fd, err_fd;
  gchar *argv[] = {
    (gchar*)sdk_path,
    "--noinform",
    "--disable-debugger",
    NULL
  };

  if (!g_spawn_async_with_pipes(
        NULL,
        argv,
        NULL,
        G_SPAWN_DO_NOT_REAP_CHILD | G_SPAWN_SEARCH_PATH,
        child_setup, NULL,
        &self->lisp_pid,
        &in_fd,
        &out_fd,
        &err_fd,
        &error))
  {
    g_print("Swank: failed to start Lisp: %s\n", error->message);
    g_clear_error(&error);
    return;
  }

  self->in_fd = in_fd;
  self->out_fd = out_fd;
  self->err_fd = err_fd;

  /* start threads to log Lisp stdout and stderr */
  g_thread_new("lisp-stdout-reader", stdout_reader_thread, self);
  g_thread_new("lisp-stderr-reader", stderr_reader_thread, GINT_TO_POINTER(err_fd));
}

static void start_swank(Swank *self)
{
  g_print("start_swank\n");
  if (self->in_fd < 0 || self->out_fd < 0)
    return;

  /* wait for initial Lisp prompt "* " */
  read_until(self, "* ");

  /* quickload swank */
  const char *ql_cmd = "(ql:quickload :swank)\n";
  g_print("start_swank: sending %s\n", ql_cmd);
  if (write(self->in_fd, ql_cmd, strlen(ql_cmd)) < 0)
    g_print("start_swank: write quickload failed: %s\n", strerror(errno));
  /* wait for quickload result and next prompt */
  read_until(self, "(:SWANK)");
  read_until(self, "* ");

  /* create swank server on port */
  char create_cmd[128];
  snprintf(create_cmd, sizeof(create_cmd),
      "(swank:create-server :port %d :dont-close t)\n",
      self->port);
  g_print("start_swank: sending %s\n", create_cmd);
  if (write(self->in_fd, create_cmd, strlen(create_cmd)) < 0)
    g_print("start_swank: write create-server failed: %s\n", strerror(errno));
  /* wait for server creation and prompt */
  read_until(self, "* ");
}

static void connect_swank(Swank *self)
{
  g_print("connect_swank: connecting to server on port %d\n", self->port);
  GSocketClient *client = g_socket_client_new();
  GError        *conn_err = NULL;
  self->connection = g_socket_client_connect_to_host(
      client, "127.0.0.1", self->port, NULL, &conn_err);
  g_object_unref(client);
  if (!self->connection) {
    g_print("connect_swank: cannot connect to server on port %d: %s\n",
        self->port, conn_err->message);
    g_clear_error(&conn_err);
    return;
  }
  g_print("connect_swank: connected.\n");

  GSocketConnection *sock = G_SOCKET_CONNECTION(self->connection);
  GSocket           *gsocket = g_socket_connection_get_socket(sock);
  self->swank_fd = g_socket_get_fd(gsocket);

  /* start thread to read Swank server replies */
  g_thread_new("swank-reader-reader", swank_reader_thread, self);
}

static void
swank_class_init(SwankClass * /*klass*/)
{
  /* no virtuals or signals */
}

static void
swank_init(Swank *self)
{
  self->lisp_pid   = 0;
  self->in_fd   = -1;
  self->out_fd  = -1;
  self->err_fd  = -1;
  self->connection = NULL;
  self->swank_fd  = -1;
  self->port       = 0;

  self->out_data = g_string_new(NULL);
  self->out_consumed = 0;
  g_mutex_init(&self->out_mutex);
  g_cond_init(&self->out_cond);
}

static void
swank_init_function(Swank *self)
{
  g_return_if_fail(GLIDE_IS_SWANK(self));
  if (self->connection)
    return;

  start_lisp(self);
  if (self->lisp_pid == 0)
    return;

  start_swank(self);
  connect_swank(self);
}

STATIC Swank *
swank_get_instance(void)
{
  if (!swank_instance) {
    g_print("Swank: get_instance triggering new()\n");
    swank_instance = g_object_new(SWANK_TYPE, NULL);
  }
  return swank_instance;
}

/**
 * Perform a remote evaluation via Swank's Emacs protocol.
 * The expression is sent as a string using (:emacs-rex).
 */
STATIC void swank_remote_execution(Swank *self, const char *expr)
{
  g_return_if_fail(GLIDE_IS_SWANK(self));

  if (!self->connection)
    swank_init_function(self);
  if (!self->connection) {
    g_print("Swank: no connection for remote execution\n");
    return;
  }

  // Escape the expression for a Lisp string literal
  gchar *escaped = escape_string(expr);

  // Build the RPC call.
  gchar *rpc = g_strdup_printf("(:emacs-rex \"%s\" \"COMMON-LISP-USER\" t 1)", escaped);
  g_free(escaped);

  /* Prefix with a 6-digit hex length header (Swank protocol) */
  size_t len         = strlen(rpc);
  const int HDR_LEN  = 6;               /* always 6 characters            */
  gchar *msg         = g_malloc(len + HDR_LEN + 1); /* +1 for terminating NUL */
  int hdr = g_snprintf(msg, HDR_LEN + 1, "%06zx", len);

  memcpy(msg + hdr, rpc, len);
  msg[HDR_LEN + len] = '\0';           /* for g_print below */

  /* Send the header (6 bytes) + payload (len bytes); skip the NUL */
  if (write(self->swank_fd, msg, HDR_LEN + len) < 0)
    g_print("swank_remote_execution: write error: %s\n", strerror(errno));

  g_print("swank_remote_execution: sent %s\n", msg);

  g_free(msg);
  g_free(rpc);
}
