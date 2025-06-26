#include "process.h"
#include "real_process.h"
#include <glib.h>
#include <string.h>

static GString *output;

static void on_out(GString *data, gpointer /*user*/) {
  g_string_append_len(output, data->str, data->len);
}

static void test_bc(void) {
  Process *p = real_process_new("/usr/bin/bc");
  g_assert_nonnull(p);
  output = g_string_new(NULL);
  process_set_stdout_cb(p, on_out, NULL);
  process_start(p);
  process_write(p, "1+2\n", -1);
  g_usleep(100000);
  g_assert_nonnull(strstr(output->str, "3"));
  process_write(p, "quit\n", -1);
  g_object_unref(p);
  g_string_free(output, TRUE);
}

static void test_no_callbacks(void) {
  Process *p = real_process_new("/usr/bin/bc");
  g_assert_nonnull(p);
  process_start(p);
  process_write(p, "1+2\n", -1);
  g_usleep(100000);
  process_write(p, "quit\n", -1);
  g_object_unref(p);
}

int main(int argc, char *argv[]) {
  g_test_init(&argc, &argv, NULL);
  g_test_add_func("/process/bc", test_bc);
  g_test_add_func("/process/no_callbacks", test_no_callbacks);
  return g_test_run();
}
