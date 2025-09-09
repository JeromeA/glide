#include "process.h"
#include <glib.h>
#include <string.h>
#include <unistd.h>
#include <sys/syscall.h>

struct _Process {
  GPid pid;
  int refcnt;
  int in_fd;
  int out_fd;
  int err_fd;
  GThread *out_thread;
  GThread *err_thread;
  ProcessCallback out_cb;
  gpointer out_user;
  ProcessCallback err_cb;
  gpointer err_user;
  gchar **argv;
  gboolean started;
};

static int intercept_fd = -1;
static int write_calls;
static gssize write_sizes[2];

ssize_t write(int fd, const void *buf, size_t count) {
  if (fd == intercept_fd) {
    write_sizes[write_calls] = count;
    write_calls++;
    if (write_calls == 1)
      return count / 2;
    return count;
  }
  return syscall(SYS_write, fd, buf, count);
}

static GString *output;

static void on_out(GString *data, gpointer /*user*/) {
  g_string_append_len(output, data->str, data->len);
}

static void test_bc(void) {
  Process *p = process_new("/usr/bin/bc");
  g_assert_nonnull(p);
  output = g_string_new(NULL);
  process_set_stdout_cb(p, on_out, NULL);
  process_start(p);
  process_write(p, "1+2\n", -1);
  g_usleep(100000);
  g_assert_nonnull(strstr(output->str, "3"));
  process_write(p, "quit\n", -1);
  process_unref(p);
  g_string_free(output, TRUE);
}

static void test_no_callbacks(void) {
  Process *p = process_new("/usr/bin/bc");
  g_assert_nonnull(p);
  process_start(p);
  process_write(p, "1+2\n", -1);
  g_usleep(100000);
  process_write(p, "quit\n", -1);
  process_unref(p);
}

static void test_lisp_no_padding(void) {
  const gchar *argv[] = {
    "sbcl", "--noinform",
    "--load", "../src/glide-package.lisp",
    "--load", "../src/server.lisp",
    "--eval", "(glide:start-server)", NULL };
  Process *p = process_new_from_argv(argv);
  g_assert_nonnull(p);
  output = g_string_new(NULL);
  process_set_stdout_cb(p, on_out, NULL);
  process_start(p);
  process_write(p,
      "(glide:eval-and-capture 1 \"(format t \"ok\")\")\n", -1);
  g_usleep(300000);
  g_assert_null(memchr(output->str, '\0', output->len));
  process_write(p, "(sb-ext:quit)\n", -1);
  process_unref(p);
  g_string_free(output, TRUE);
}

static void test_partial_write(void) {
  struct _Process p = {0};
  intercept_fd = 99;
  p.in_fd = intercept_fd;
  write_calls = 0;
  gboolean ok = process_write(&p, "abcdef", 6);
  g_assert_true(ok);
  g_assert_cmpint(write_calls, ==, 2);
  g_assert_cmpint(write_sizes[0], ==, 6);
  g_assert_cmpint(write_sizes[1], ==, 3);
  intercept_fd = -1;
}

int main(int argc, char *argv[]) {
  g_test_init(&argc, &argv, NULL);
  g_test_add_func("/process/bc", test_bc);
  g_test_add_func("/process/no_callbacks", test_no_callbacks);
  g_test_add_func("/process/lisp_no_padding", test_lisp_no_padding);
  g_test_add_func("/process/partial_write", test_partial_write);
  return g_test_run();
}
