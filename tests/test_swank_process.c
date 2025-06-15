#include "swank_process.h"
#include "process.h"
#include <glib.h>
#include <sys/socket.h>
#include <unistd.h>
#include <string.h>

typedef struct {
    ProcessImpl base;
    int fd;
} MockProcess;

static void mp_set_out(ProcessImpl *p, ProcessCallback cb, gpointer data) { (void)p; }
static void mp_set_err(ProcessImpl *p, ProcessCallback cb, gpointer data) { (void)p; }
static gboolean mp_write(ProcessImpl *p, const gchar *d, gssize len) {
    MockProcess *mp = (MockProcess*)p;
    if (len < 0) len = strlen(d);
    return write(mp->fd, d, len) == len;
}
static void mp_free(ProcessImpl *p) { MockProcess *mp = (MockProcess*)p; close(mp->fd); g_free(mp); }

static const Process mp_iface = { mp_set_out, mp_set_err, mp_write, mp_free };

typedef struct {
    SwankProcessImpl base;
    ProcessImpl *proc;
    void *prefs;
    int swank_fd;
    void *conn;
    GString *out_data; gsize out_consumed; GMutex out_mutex; GCond out_cond;
    GString *swank_data; gsize swank_consumed; GMutex swank_mutex; GCond swank_cond;
    int port; GThread *swank_thread;
} RealSwankProcess;

static void test_send_receive(void)
{
    int fds[2];
    g_assert_cmpint(socketpair(AF_UNIX, SOCK_STREAM, 0, fds), ==, 0);
    MockProcess *mp = g_new0(MockProcess,1);
    mp->base.iface = &mp_iface;
    mp->fd = fds[0];
    SwankProcessImpl *sp = swank_process_new((ProcessImpl*)mp, NULL);
    ((RealSwankProcess*)sp)->swank_fd = fds[0];

    GString *payload = g_string_new("hello");
    swank_process_send(sp, payload);
    g_string_free(payload, TRUE);

    char buf[64];
    ssize_t n = read(fds[1], buf, sizeof(buf));
    g_assert_cmpint(n, ==, 11); /* 6 hdr + 5 */
    buf[n] = '\0';
    g_assert_nonnull(strstr(buf, "hello"));

    /* skip receiving to keep test simple */

    swank_process_free(sp);
    close(fds[1]);
}

int main(int argc, char *argv[])
{
    g_test_init(&argc, &argv, NULL);
    g_test_add_func("/swank/send_receive", test_send_receive);
    return g_test_run();
}
