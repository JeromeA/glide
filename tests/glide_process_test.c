#include "real_glide_process.h"
#include "process.h"
#include <glib.h>
#include <sys/socket.h>
#include <unistd.h>
#include <string.h>

typedef struct {
  Process base;
  int fd;
  ProcessCallback out_cb;
  gpointer out_data;
} MockProcess;

static void mp_set_out(Process *p, ProcessCallback cb, gpointer data) {
  MockProcess *mp = (MockProcess*)p;
  mp->out_cb = cb;
  mp->out_data = data;
}
static void mp_set_err(Process *p, ProcessCallback cb, gpointer data) { (void)p; (void)cb; (void)data; }
static void mp_start(Process *p) {
  MockProcess *mp = (MockProcess*)p;
  if (mp->out_cb) {
    GString *line = g_string_new("banner\n");
    mp->out_cb(line, mp->out_data);
    g_string_free(line, TRUE);
    line = g_string_new("* \n");
    mp->out_cb(line, mp->out_data);
    g_string_free(line, TRUE);
  }
}
static gboolean mp_write(Process *p, const gchar *d, gssize len) {
  MockProcess *mp = (MockProcess*)p;
  if (len < 0) len = strlen(d);
  if (mp->fd >= 0)
    write(mp->fd, d, len);
  if (mp->out_cb) {
    if (strstr(d, "(require :glide)")) {
      GString *line = g_string_new("NIL\n");
      mp->out_cb(line, mp->out_data);
      g_string_free(line, TRUE);
      line = g_string_new("* \n");
      mp->out_cb(line, mp->out_data);
      g_string_free(line, TRUE);
    } else if (strstr(d, "(glide:start-server)")) {
      GString *line = g_string_new("READY\n");
      mp->out_cb(line, mp->out_data);
      g_string_free(line, TRUE);
    }
  }
  return TRUE;
}
static void mp_destroy(Process *p) {
  MockProcess *mp = (MockProcess*)p;
  if (mp->fd >= 0) close(mp->fd);
  g_free(mp);
}

static const ProcessOps mock_process_ops = {
  .start = mp_start,
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
  mp->out_cb = NULL;
  mp->out_data = NULL;
  return mp;
}

static void test_send(void) {
  int fds[2];
  g_assert_cmpint(socketpair(AF_UNIX, SOCK_STREAM, 0, fds), ==, 0);
  MockProcess *mp = mock_process_new();
  mp->fd = fds[0];

  GlideProcess *sp = real_glide_process_new((Process*)mp);

  GString *payload = g_string_new("hello");
  glide_process_send(sp, payload);
  g_string_free(payload, TRUE);

  char buf[64];
  ssize_t n = read(fds[1], buf, sizeof(buf));
  g_assert_cmpint(n, ==, 5);
  buf[n] = '\0';
  g_assert_nonnull(strstr(buf, "hello"));

  glide_process_unref(sp);
  process_unref((Process*)mp);
  close(fds[1]);
}

static void on_start_msg(GString *msg, gpointer user_data) {
  GPtrArray *msgs = user_data;
  g_ptr_array_add(msgs, g_strdup(msg->str));
}

static void test_start_consumes_prompts(void) {
  MockProcess *mp = mock_process_new();
  GlideProcess *sp = real_glide_process_new((Process*)mp);
  GPtrArray *msgs = g_ptr_array_new_with_free_func(g_free);
  glide_process_set_message_cb(sp, on_start_msg, msgs);
  glide_process_start(sp);
  g_assert_cmpint(msgs->len, ==, 1);
  g_assert_cmpstr(g_ptr_array_index(msgs, 0), ==, "READY");
  g_ptr_array_free(msgs, TRUE);
  glide_process_unref(sp);
  process_unref((Process*)mp);
}

int main(int argc, char *argv[]) {
  g_test_init(&argc, &argv, NULL);
  g_test_add_func("/glide/send", test_send);
  g_test_add_func("/glide/start_prompts", test_start_consumes_prompts);
  return g_test_run();
}
