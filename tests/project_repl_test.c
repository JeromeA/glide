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
  project_request_describe(project, "COMMON-LISP", "WHEN");
  project_request_describe(project, "COMMON-LISP", "IF");

  Function *fn = NULL;
  Function *macro_fn = NULL;
  Function *special_fn = NULL;
  const gchar *var_doc = NULL;
  for (int i = 0; i < 200; i++) {
    g_main_context_iteration(NULL, FALSE);
    fn = project_get_function(project, "FOO");
    macro_fn = project_get_function(project, "WHEN");
    special_fn = project_get_function(project, "IF");
    var_doc = project_get_variable(project, "*BAR*");
    if (fn && macro_fn && special_fn && var_doc)
      break;
    g_usleep(100000);
  }
  g_assert_nonnull(fn);
  g_assert_cmpstr(function_get_doc_string(fn), ==, "foo doc");
  g_assert_cmpint(function_get_kind(fn), ==, FUNCTION_KIND_COMPILED_FUNCTION);
  const Node *lambda = function_get_lambda_list(fn);
  g_assert_nonnull(lambda);
  g_assert_cmpint(lambda->type, ==, LISP_AST_NODE_TYPE_LIST);
  g_assert_nonnull(macro_fn);
  g_assert_cmpint(function_get_kind(macro_fn), ==, FUNCTION_KIND_MACRO);
  const Node *macro_lambda = function_get_lambda_list(macro_fn);
  g_assert_nonnull(macro_lambda);
  g_assert_cmpint(macro_lambda->type, ==, LISP_AST_NODE_TYPE_LIST);
  const gchar *macro_doc = function_get_doc_string(macro_fn);
  g_assert_nonnull(macro_doc);
  g_assert_nonnull(g_strrstr(macro_doc, "rest of the forms"));
  g_assert_nonnull(special_fn);
  g_assert_cmpint(function_get_kind(special_fn), ==,
      FUNCTION_KIND_SPECIAL_OPERATOR);
  const Node *special_lambda = function_get_lambda_list(special_fn);
  g_assert_nonnull(special_lambda);
  g_assert_cmpint(special_lambda->type, ==, LISP_AST_NODE_TYPE_LIST);
  const gchar *special_doc = function_get_doc_string(special_fn);
  g_assert_nonnull(special_doc);
  g_assert_nonnull(g_strrstr(special_doc, "IF predicate then [else]"));
  g_assert_nonnull(g_strrstr(special_doc, "otherwise evaluate ELSE"));
  g_assert_nonnull(var_doc);
  g_assert_cmpstr(var_doc, ==, "bar doc");

  project_unref(project);
  repl_session_unref(sess);
  repl_process_unref(rp);
  status_service_free(status_service);
}

int main(int argc, char *argv[]) {
  g_test_init(&argc, &argv, NULL);
  GMainContext *ctx = g_main_context_default();
  g_main_context_push_thread_default(ctx);
  g_test_add_func("/project_repl/describe", test_describe);
  int ret = g_test_run();
  g_main_context_pop_thread_default(ctx);
  return ret;
}
