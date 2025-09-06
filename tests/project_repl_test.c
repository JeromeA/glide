#include "project_repl.h"
#include "repl_test_helpers.h"
#include "function.h"
#include <glib.h>

static void test_describe(void) {
  ReplProcess *rp;
  StatusService *status_service;
  ReplSession *sess;
  Project *project = build_project(&sess, &rp, &status_service);

  eval_form(sess, "(defun foo () \"foo doc\" 42)");
  eval_form(sess, "(defparameter *bar* 42 \"bar doc\")");
  eval_form(sess, "(export '(foo *bar*))");

  project_request_describe(project, "COMMON-LISP-USER", "FOO");
  project_request_describe(project, "COMMON-LISP-USER", "*BAR*");

  Function *fn = NULL;
  const gchar *var_doc = NULL;
  for (int i = 0; i < 200; i++) {
    fn = project_get_function(project, "FOO");
    var_doc = project_get_variable(project, "*BAR*");
    if (fn && var_doc)
      break;
    g_usleep(100000);
  }
  g_assert_nonnull(fn);
  g_assert_cmpstr(function_get_doc_string(fn), ==, "foo doc");
  g_assert_nonnull(var_doc);
  g_assert_cmpstr(var_doc, ==, "bar doc");

  project_unref(project);
  repl_session_unref(sess);
  repl_process_unref(rp);
  status_service_free(status_service);
}

int main(int argc, char *argv[]) {
  g_test_init(&argc, &argv, NULL);
  g_test_add_func("/project_repl/describe", test_describe);
  return g_test_run();
}
