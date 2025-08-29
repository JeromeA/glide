#include <glib.h>

#include "../src/repl_process.c"

static GPtrArray *messages;

static void on_msg(GString *msg, gpointer /*user*/) {
  g_ptr_array_add(messages, g_string_new_len(msg->str, msg->len));
}

static void test_messages(void) {
  ReplProcess *rp = repl_process_new(NULL);
  messages = g_ptr_array_new_with_free_func((GDestroyNotify)g_string_free);
  repl_process_set_message_cb(rp, on_msg, NULL);

  GString *chunk = g_string_new("(:first");
  on_proc_out(chunk, rp);
  g_string_free(chunk, TRUE);
  g_assert_cmpuint(messages->len, ==, 0);

  chunk = g_string_new(")\n(:second \"a\\\" )b\")");
  on_proc_out(chunk, rp);
  g_string_free(chunk, TRUE);
  g_assert_cmpuint(messages->len, ==, 2);
  g_assert_cmpstr(((GString*)messages->pdata[0])->str, ==, "(:first)");
  g_assert_cmpstr(((GString*)messages->pdata[1])->str, ==, "(:second \"a\\\" )b\")");

  chunk = g_string_new("\n(:third\n:fourth)");
  on_proc_out(chunk, rp);
  g_string_free(chunk, TRUE);
  g_assert_cmpuint(messages->len, ==, 3);
  g_assert_cmpstr(((GString*)messages->pdata[2])->str, ==, "(:third\n:fourth)");

  repl_process_unref(rp);
  g_ptr_array_unref(messages);
}

int main(int argc, char *argv[]) {
  g_test_init(&argc, &argv, NULL);
  g_test_add_func("/repl_process/messages", test_messages);
  return g_test_run();
}
