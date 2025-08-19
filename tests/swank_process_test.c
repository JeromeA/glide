#include "real_swank_process.h"
#include "process.h"
#include <glib.h>
#include <sys/socket.h>
#include <unistd.h>
#include <string.h>

typedef struct {
  Process base;
  int fd;
} MockProcess;

static void mp_set_out(Process *p, ProcessCallback cb, gpointer data) { (void)p; (void)cb; (void)data; }
static void mp_set_err(Process *p, ProcessCallback cb, gpointer data) { (void)p; (void)cb; (void)data; }
static gboolean mp_write(Process *p, const gchar *d, gssize len) {
  MockProcess *mp = (MockProcess*)p;
  if (len < 0) len = strlen(d);
  return write(mp->fd, d, len) == len;
}
static void mp_destroy(Process *p) {
  MockProcess *mp = (MockProcess*)p;
  if (mp->fd >= 0) close(mp->fd);
  g_free(mp);
}

static const ProcessOps mock_process_ops = {
  .start = NULL,
  .set_stdout_cb = mp_set_out,
  .set_stderr_cb = mp_set_err,
  .write = mp_write,
  .destroy = mp_destroy,
};

static MockProcess *mock_process_new(void) {
  MockProcess *mp = g_new0(MockProcess, 1);
  mp->base.ops = &mock_process_ops;
  mp->base.refcnt = 1;
  mp->fd = -1;
  return mp;
}

static void test_send(void) {
  int fds[2];
  g_assert_cmpint(socketpair(AF_UNIX, SOCK_STREAM, 0, fds), ==, 0);
  MockProcess *mp = mock_process_new();
  mp->fd = fds[0];

  SwankProcess *sp = real_swank_process_new((Process*)mp, NULL);
  real_swank_process_set_socket((RealSwankProcess*)sp, fds[0]);

  GString *payload = g_string_new("hello");
  swank_process_send(sp, payload);
  g_string_free(payload, TRUE);

  char buf[64];
  ssize_t n = read(fds[1], buf, sizeof(buf));
  g_assert_cmpint(n, ==, 11);
  buf[n] = '\0';
  g_assert_nonnull(strstr(buf, "hello"));

  swank_process_unref(sp);
  process_unref((Process*)mp);
  close(fds[1]);
}

int main(int argc, char *argv[]) {
  g_test_init(&argc, &argv, NULL);
  g_test_add_func("/swank/send", test_send);
  return g_test_run();
}
