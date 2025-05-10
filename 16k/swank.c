#include "swank.h"
#include "preferences.h"

#include <gio/gio.h>
#include <glib.h>
#include <unistd.h>
#include <string.h>
#include <errno.h>
#include <stdlib.h>
#include <stdio.h>

#define DEFAULT_SWANK_PORT 4005

struct _Swank {
    GObject             parent_instance;
    GPid                lisp_pid;
    int                 stdin_fd;
    int                 stdout_fd;
    int                 stderr_fd;
    GSocketConnection  *connection;
    int                 socket_fd;
    int                 port;
};

G_DEFINE_TYPE(Swank, swank, G_TYPE_OBJECT)

static Swank *swank_instance = NULL;

/* Thread to read from a file descriptor and print lines */
static gpointer
reader_thread(gpointer data)
{
    int fd = GPOINTER_TO_INT(data);
    char buf[1024];
    ssize_t n;

    while ((n = read(fd, buf, sizeof(buf) - 1)) > 0) {
        buf[n] = '\0';
        g_print("%d: %s\n", fd, buf);
    }
    return NULL;
}

/* Thread to read swank replies */
static gpointer
swank_reply_thread(gpointer data)
{
    int fd = GPOINTER_TO_INT(data);
    char buf[2048];
    ssize_t n;

    while ((n = read(fd, buf, sizeof(buf) - 1)) > 0) {
        buf[n] = '\0';
        g_print("Swank reply: %s", buf);
    }
    return NULL;
}

/* Helper: read from fd until a pattern appears, logging all data */
static void
read_until(int fd, const char *pattern)
{
    char buf[1024];
    GString *acc = g_string_new(NULL);
    ssize_t n;

    while ((n = read(fd, buf, sizeof(buf) - 1)) > 0) {
        buf[n] = '\0';
        g_string_append(acc, buf);
        if (strstr(acc->str, pattern))
            break;
    }
    g_print("Swank init data: %s\n", acc->str);
    g_string_free(acc, TRUE);
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

static void start_lisp(Swank *self)
{
    Preferences *prefs = preferences_get_instance();
    const gchar *sdk_path = preferences_get_sdk(prefs);
    if (!sdk_path) {
        g_warning("Swank: SDK not configured");
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
            NULL, NULL,
            &self->lisp_pid,
            &in_fd,
            &out_fd,
            &err_fd,
            &error))
    {
        g_warning("Swank: failed to start Lisp: %s", error->message);
        g_clear_error(&error);
        return;
    }

    self->stdin_fd  = in_fd;
    self->stdout_fd = out_fd;
    self->stderr_fd = err_fd;

    /* start threads to log Lisp stdout and stderr */
    g_thread_new("lisp-stdout-reader", reader_thread, GINT_TO_POINTER(out_fd));
    g_thread_new("lisp-stderr-reader", reader_thread, GINT_TO_POINTER(err_fd));
}

static void start_swank(Swank *self)
{
    if (self->stdin_fd < 0 || self->stdout_fd < 0)
        return;

    /* wait for initial Lisp prompt "* " */
    read_until(self->stdout_fd, "* ");

    /* quickload swank */
    const char *ql_cmd = "(ql:quickload :swank)\n";
    if (write(self->stdin_fd, ql_cmd, strlen(ql_cmd)) < 0)
        g_warning("Swank: write quickload failed: %s", strerror(errno));
    /* wait for quickload result and next prompt */
    read_until(self->stdout_fd, "(:SWANK)");
    read_until(self->stdout_fd, "* ");

    /* create swank server on port */
    char create_cmd[128];
    snprintf(create_cmd, sizeof(create_cmd),
             "(swank:create-server :port %d :dont-close t)\n",
             self->port);
    if (write(self->stdin_fd, create_cmd, strlen(create_cmd)) < 0)
        g_warning("Swank: write create-server failed: %s", strerror(errno));
    /* wait for server creation and prompt */
    read_until(self->stdout_fd, "* ");
}

static void connect_swank(Swank *self)
{
    GSocketClient *client = g_socket_client_new();
    GError        *conn_err = NULL;
    self->connection = g_socket_client_connect_to_host(
        client, "127.0.0.1", self->port, NULL, &conn_err);
    g_object_unref(client);
    if (!self->connection) {
        g_warning("Swank: cannot connect to server on port %d: %s",
                  self->port, conn_err->message);
        g_clear_error(&conn_err);
        return;
    }

    GSocketConnection *sock = G_SOCKET_CONNECTION(self->connection);
    GSocket           *gsocket = g_socket_connection_get_socket(sock);
    self->socket_fd = g_socket_get_fd(gsocket);

    /* start thread to read Swank server replies */
    g_thread_new("swank-reply-reader",
                 swank_reply_thread,
                 GINT_TO_POINTER(self->socket_fd));
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
    self->stdin_fd   = -1;
    self->stdout_fd  = -1;
    self->stderr_fd  = -1;
    self->connection = NULL;
    self->socket_fd  = -1;
    self->port       = 0;
}

Swank *
swank_get_instance(void)
{
    if (!swank_instance) {
        swank_instance = g_object_new(SWANK_TYPE, NULL);
    }
    return swank_instance;
}

void
swank_init_function(Swank *self)
{
    g_return_if_fail(SWANK_IS_OBJECT(self));
    if (self->connection)
        return;

    start_lisp(self);
    if (self->lisp_pid == 0)
        return;

    start_swank(self);
    connect_swank(self);
}

/**
 * Perform a remote evaluation via Swank's Emacs protocol.
 * The expression is sent as a string using (swank:emacs-rex).
 */
void
swank_remote_execution(Swank *self, const char *expr)
{
    g_return_if_fail(SWANK_IS_OBJECT(self));

    if (!self->connection)
        swank_init_function(self);
    if (!self->connection) {
        g_warning("Swank: no connection for remote execution");
        return;
    }

    /* Escape the expression for Lisp string literal */
    gchar *escaped = escape_string(expr);
    
    /* Build the RPC call: (swank:emacs-rex "<expr>" "COMMON-LISP-USER" t 1) */
    gchar *rpc = g_strdup_printf(
        "(swank:emacs-rex \"%s\" \"COMMON-LISP-USER\" t 1)\n",
        escaped);
    g_free(escaped);

    /* Prefix with length header */
    size_t len = strlen(rpc);
    gchar *msg = g_malloc(len + 32);
    int hdr = g_snprintf(msg, 32, "%zu:", len);
    memcpy(msg + hdr, rpc, len);

    /* Send it */
    if (write(self->socket_fd, msg, hdr + len) < 0)
        g_warning("Swank: write error: %s", strerror(errno));

    g_free(msg);
    g_free(rpc);
}
