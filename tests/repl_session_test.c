#include "repl_session.h"
#include "repl_process.h"
#include "process.h"
#include "status_service.h"
#include "interaction.h"
#include <glib.h>

static void test_eval(void) {
  const gchar *argv[] = {
    "sbcl", "--noinform",
    "--eval", "(require :asdf)",
    "--eval", "(pushnew (truename \"../src/\") asdf:*central-registry*)",
    NULL
  };
  Process *proc = process_new_from_argv(argv);
  g_assert_nonnull(proc);
  ReplProcess *gp = repl_process_new(proc);
  process_unref(proc);
  StatusService *status_service = status_service_new();
  ReplSession *sess = repl_session_new(gp, status_service);
  Interaction interaction;
  interaction_init(&interaction, "(+ 1 2)");
  repl_session_eval(sess, &interaction);
  int tries = 0;
  while ((interaction.status == INTERACTION_CREATED || interaction.status == INTERACTION_RUNNING) && tries++ < 100) {
    g_usleep(100000);
  }
  g_assert_cmpint(interaction.status, ==, INTERACTION_OK);
  g_assert_cmpstr(interaction.result, ==, "3");
  interaction_clear(&interaction);
  repl_session_unref(sess);
  repl_process_unref(gp);
  status_service_free(status_service);
}

int main(int argc, char *argv[]) {
  g_test_init(&argc, &argv, NULL);
  g_test_add_func("/repl_session/eval", test_eval);
  return g_test_run();
}
