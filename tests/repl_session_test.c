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
    "--eval", "(let ((p (probe-file \"../src/\"))) (when p (pushnew p asdf:*central-registry*)))",
    "--eval", "(let ((p (probe-file \"src/\"))) (when p (pushnew p asdf:*central-registry*)))",
    "--eval", "(setf *load-verbose* nil *compile-verbose* nil)",
    "--eval", "(require :glide)",
    "--eval", "(glide:start-server)",
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
  while (tries++ < 200) {
    g_mutex_lock(&interaction.lock);
    InteractionStatus status = interaction.status;
    g_mutex_unlock(&interaction.lock);
    if (status != INTERACTION_CREATED && status != INTERACTION_RUNNING)
      break;
    g_usleep(100000);
  }
  g_mutex_lock(&interaction.lock);
  InteractionStatus status = interaction.status;
  gchar *result = interaction.result ? g_strdup(interaction.result->str) : NULL;
  g_mutex_unlock(&interaction.lock);
  g_assert_cmpint(status, ==, INTERACTION_OK);
  g_assert_cmpstr(result, ==, "3");
  g_free(result);
  interaction_clear(&interaction);
  repl_session_unref(sess);
  repl_process_unref(gp);
  status_service_free(status_service);
}

static void test_ignore_compilation_comment(void) {
  Process *proc = process_new("/bin/true");
  ReplProcess *rp = repl_process_new(proc);
  process_unref(proc);
  StatusService *status_service = status_service_new();
  ReplSession *sess = repl_session_new(rp, status_service);
  repl_process_unref(rp);
  Interaction interaction;
  interaction_init(&interaction, "42");
  repl_session_eval(sess, &interaction);
  guint32 tag = 0;
  int tries = 0;
  while (tries++ < 50) {
    g_mutex_lock(&interaction.lock);
    if (interaction.status == INTERACTION_RUNNING) {
      tag = interaction.tag;
      g_mutex_unlock(&interaction.lock);
      break;
    }
    g_mutex_unlock(&interaction.lock);
    g_usleep(100000);
  }
  gchar *payload = g_strdup_printf("; wrote tmp.fasl\n(result %u 42)\n", tag);
  GString *msg = g_string_new(payload);
  g_free(payload);
  repl_session_on_message(msg, sess);
  g_string_free(msg, TRUE);
  g_mutex_lock(&interaction.lock);
  g_assert_cmpint(interaction.status, ==, INTERACTION_OK);
  g_assert(interaction.result);
  g_assert_cmpstr(interaction.result->str, ==, "42");
  g_mutex_unlock(&interaction.lock);
  interaction_clear(&interaction);
  repl_session_unref(sess);
  status_service_free(status_service);
}

int main(int argc, char *argv[]) {
  g_test_init(&argc, &argv, NULL);
  g_test_add_func("/repl_session/eval", test_eval);
  g_test_add_func("/repl_session/ignore_compilation_comment", test_ignore_compilation_comment);
  return g_test_run();
}
