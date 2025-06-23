#include "process.h"
#include <glib.h>
#include <string.h>

static GString *output;

static void on_out(GString *data, gpointer user_data) {
    g_string_append_len(output, data->str, data->len);
}

static void test_bc(void)
{
  ProcessImpl *p = process_new("/usr/bin/bc");
  g_assert_nonnull(p);
  output = g_string_new(NULL);
  process_set_stdout_cb(p, on_out, NULL);
  process_write(p, "1+2\n", -1);
  g_usleep(100000);
  g_assert_nonnull(strstr(output->str, "3"));
  process_write(p, "quit\n", -1);
  process_free(p);
  g_string_free(output, TRUE);
}

int main(int argc, char *argv[])
{
    g_test_init(&argc, &argv, NULL);
    g_test_add_func("/process/bc", test_bc);
    return g_test_run();
}
