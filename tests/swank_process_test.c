#include "real_swank_process.h"
#include "process.h"
#include <glib.h>
#include <sys/socket.h>
#include <unistd.h>
#include <string.h>

typedef struct {
  GObject parent_instance;
  int fd;
} MockProcess;

typedef struct { GObjectClass parent_class; } MockProcessClass;

static void mp_set_out(Process *p, ProcessCallback cb, gpointer data) { (void)p; (void)cb; (void)data; }
static void mp_set_err(Process *p, ProcessCallback cb, gpointer data) { (void)p; (void)cb; (void)data; }
static gboolean mp_write(Process *p, const gchar *d, gssize len) {
  MockProcess *mp = (MockProcess*)p;
  if (len < 0) len = strlen(d);
  return write(mp->fd, d, len) == len;
}

static void mock_process_iface_init(ProcessInterface *iface) {
  iface->set_stdout_cb = mp_set_out;
  iface->set_stderr_cb = mp_set_err;
  iface->write = mp_write;
}

G_DEFINE_TYPE_WITH_CODE(MockProcess, mock_process, G_TYPE_OBJECT,
    G_IMPLEMENT_INTERFACE(PROCESS_TYPE, mock_process_iface_init))

static void mock_process_finalize(GObject *obj) {
  MockProcess *mp = (MockProcess*)obj;
  if (mp->fd >= 0) close(mp->fd);
  G_OBJECT_CLASS(mock_process_parent_class)->finalize(obj);
}

static void mock_process_class_init(MockProcessClass *klass) {
  GObjectClass *obj = G_OBJECT_CLASS(klass);
  obj->finalize = mock_process_finalize;
}

static void mock_process_init(MockProcess *self) {
  self->fd = -1;
}

static void test_send(void) {
  int fds[2];
  g_assert_cmpint(socketpair(AF_UNIX, SOCK_STREAM, 0, fds), ==, 0);
  MockProcess *mp = g_object_new(mock_process_get_type(), NULL);
  mp->fd = fds[0];

  SwankProcess *sp = real_swank_process_new(GLIDE_PROCESS(mp), NULL);
  real_swank_process_set_socket(GLIDE_REAL_SWANK_PROCESS(sp), fds[0]);

  GString *payload = g_string_new("hello");
  swank_process_send(sp, payload);
  g_string_free(payload, TRUE);

  char buf[64];
  ssize_t n = read(fds[1], buf, sizeof(buf));
  g_assert_cmpint(n, ==, 11);
  buf[n] = '\0';
  g_assert_nonnull(strstr(buf, "hello"));

  g_object_unref(sp);
  g_object_unref(mp);
  close(fds[1]);
}

int main(int argc, char *argv[]) {
  g_test_init(&argc, &argv, NULL);
  g_test_add_func("/swank/send", test_send);
  return g_test_run();
}
