#include "project.h"
#include "node.h"
#include "function.h"
#include <glib.h>
#include <glib/gstdio.h>
#include <string.h>

static void document_prepare(Document *document) {
  g_return_if_fail(document);
  const GString *content = document_get_content(document);
  GArray *tokens = lisp_lexer_lex(content);
  Node *ast = lisp_parser_parse(tokens, document);
  document_set_tokens(document, tokens);
  document_set_ast(document, ast);
}

static void document_set_text(Document *document, const gchar *text) {
  g_return_if_fail(document);
  g_return_if_fail(text);
  document_set_content(document, g_string_new(text));
  document_prepare(document);
}

static Document *create_virtual_file(const gchar *text) {
  Document *document = document_new_virtual(g_string_new(text));
  document_prepare(document);
  return document;
}

static Function *create_function_with_lambda(const gchar *name,
    const gchar *lambda) {
  Document *document = create_virtual_file(lambda);
  const Node *ast = document_get_ast(document);
  Node *lambda_node = NULL;
  if (ast && ast->children && ast->children->len > 0)
    lambda_node = g_array_index(ast->children, Node*, 0);
  return function_new(NULL, lambda_node, NULL, NULL, FUNCTION_KIND_FUNCTION,
      name, "CL-USER", document);
}

static void test_default_file(void)
{
  Project *project = project_new(NULL);
  g_assert_cmpuint(project_get_document_count(project), ==, 1);
  Document *document = project_get_document(project, 0);
  g_assert_cmpint(document_get_state(document), ==, DOCUMENT_LIVE);
  g_assert_cmpstr(document_get_path(document), ==, "unnamed.lisp");
  project_unref(project);
}

static void test_parse_on_change(void)
{
  Project *project = project_new(NULL);
  Document *document = project_add_document(project, g_string_new("(a)"), NULL, DOCUMENT_LIVE);
  project_document_changed(project, document);
  const GArray *tokens = document_get_tokens(document);
  g_assert_cmpint(tokens->len, ==, 3); /* (, a, ) */
  const LispToken *token = &g_array_index(tokens, LispToken, 0);
  g_assert_cmpint(token->type, ==, LISP_TOKEN_TYPE_LIST_START);
  project_document_changed(project, document); /* should still parse without error */
  const Node *ast = document_get_ast(document);
  g_assert_cmpint(ast->children->len, ==, 1);
  project_document_changed(project, document);
  project_unref(project);
}

static void on_file_loaded(Project * /*project*/, Document * /*document*/,
    gpointer user_data)
{
  int *count = user_data;
  (*count)++;
}

static void test_file_load(void)
{
  gchar *tmpdir = g_dir_make_tmp("project-test-XXXXXX", NULL);
  gchar *filepath = g_build_filename(tmpdir, "document.lisp", NULL);
  const gchar *contents = "(a b c)";
  g_file_set_contents(filepath, contents, -1, NULL);

  Project *project = project_new(NULL);

  int count = 0;
  project_set_document_loaded_cb(project, on_file_loaded, &count);

  Document *document = project_add_loaded_document(project, filepath);
  g_assert_nonnull(document);
  g_assert_cmpint(count, ==, 1);

  const GString *content = document_get_content(document);
  g_assert_nonnull(content);
  g_assert_cmpstr(content->str, ==, contents);

  project_unref(project);
  g_remove(filepath);
  g_rmdir(tmpdir);
  g_free(filepath);
  g_free(tmpdir);
}

static void test_function_analysis(void)
{
  Project *project = project_new(NULL);
  Document *document = project_add_document(project, g_string_new("(defun foo () (bar))"), NULL, DOCUMENT_LIVE);
  project_document_changed(project, document);
  const Node *ast = document_get_ast(document);
  const Node *form = g_array_index(ast->children, Node*, 0);
  Node *defsym_symbol = g_array_index(form->children, Node*, 0);
  Node *defsym = node_get_symbol_name_node(defsym_symbol);
  Node *name_symbol = g_array_index(form->children, Node*, 1);
  Node *name = node_get_symbol_name_node(name_symbol);
  Node *call = g_array_index(form->children, Node*, 3);
  Node *callee_symbol = g_array_index(call->children, Node*, 0);
  Node *callee = node_get_symbol_name_node(callee_symbol);
  g_assert_nonnull(defsym);
  g_assert_nonnull(name);
  g_assert_nonnull(callee);
  g_assert_true(node_is(defsym, SDT_FUNCTION_USE));
  g_assert_true(node_is(name, SDT_FUNCTION_DEF));
  g_assert_true(node_is(callee, SDT_FUNCTION_USE));
  project_unref(project);
}

static void test_index(void)
{
  Project *project = project_new(NULL);
  Document *document = project_add_document(project, g_string_new("(defun foo () (bar))"), NULL,
      DOCUMENT_LIVE);
  project_document_changed(project, document);
  const Node *ast = document_get_ast(document);
  const Node *form = g_array_index(ast->children, Node*, 0);
  Node *defsym_symbol = g_array_index(form->children, Node*, 0);
  Node *defsym = node_get_symbol_name_node(defsym_symbol);
  Node *name_symbol = g_array_index(form->children, Node*, 1);
  Node *name = node_get_symbol_name_node(name_symbol);
  Node *call = g_array_index(form->children, Node*, 3);
  Node *callee_symbol = g_array_index(call->children, Node*, 0);
  Node *callee = node_get_symbol_name_node(callee_symbol);
  g_assert_nonnull(defsym);
  g_assert_nonnull(name);
  g_assert_nonnull(callee);
  GHashTable *defs = project_get_index(project, SDT_FUNCTION_DEF);
  GHashTable *uses = project_get_index(project, SDT_FUNCTION_USE);
  GPtrArray *defarr = g_hash_table_lookup(defs, "FOO");
  g_assert_nonnull(defarr);
  g_assert_cmpuint(defarr->len, ==, 1);
  g_assert_true(g_ptr_array_index(defarr, 0) == name);
  GPtrArray *use_defun = g_hash_table_lookup(uses, "DEFUN");
  GPtrArray *use_bar = g_hash_table_lookup(uses, "BAR");
  g_assert_nonnull(use_defun);
  g_assert_nonnull(use_bar);
  g_assert_cmpuint(use_defun->len, ==, 1);
  g_assert_cmpuint(use_bar->len, ==, 1);
  g_assert_true(g_ptr_array_index(use_defun, 0) == defsym);
  g_assert_true(g_ptr_array_index(use_bar, 0) == callee);
  project_unref(project);
}

static void test_functions_table(void)
{
  Project *project = project_new(NULL);
  Document *document = project_add_document(project, g_string_new("(defun foo () \"doc\")"), NULL, DOCUMENT_LIVE);
  project_document_changed(project, document);
  Function *fn = project_get_function(project, "FOO");
  g_assert_nonnull(fn);
  const gchar *doc = function_get_doc_string(fn);
  g_assert_nonnull(doc);
  g_assert_cmpstr(doc, ==, "doc");
  g_assert_cmpint(function_get_kind(fn), ==, FUNCTION_KIND_FUNCTION);
  g_assert_cmpstr(function_get_name(fn), ==, "FOO");
  g_assert_cmpstr(function_get_package(fn), ==, "CL-USER");
  project_unref(project);
}

static void test_function_tooltip(void)
{
  Project *project = project_new(NULL);
  Document *document = project_add_document(project, g_string_new("(defun foo (x &rest rest) \"doc\")"), NULL, DOCUMENT_LIVE);
  project_document_changed(project, document);
  Function *fn = project_get_function(project, "FOO");
  gchar *tooltip = function_tooltip(fn);
  g_assert_nonnull(tooltip);
  g_assert_cmpstr(tooltip, ==,
      "<span foreground=\"brown\"><b>FOO</b></span> is a function in <span foreground=\"darkgreen\">CL-USER</span>:\n"
      "(<span foreground=\"brown\"><b>FOO</b></span> X <span foreground=\"darkgreen\">&amp;REST</span> REST)\n\n"
      "doc");
  g_free(tooltip);
  project_unref(project);
}

static void test_defun_requires_symbol_name(void)
{
  Project *project = project_new(NULL);
  Document *document = project_add_document(project, g_string_new("(defun \"foo\" () nil)"), NULL, DOCUMENT_LIVE);
  project_document_changed(project, document);

  const GArray *errors = document_get_errors(document);
  g_assert_nonnull(errors);
  g_assert_cmpuint(errors->len, ==, 1);
  const DocumentError *err = &g_array_index((GArray*)errors, DocumentError, 0);
  g_assert_nonnull(err->message);
  g_assert_cmpstr(err->message, ==, "DEFUN requires a symbol name.");
  g_assert_cmpint(err->type, ==, DOCUMENT_ERROR_TYPE_GENERIC);

  project_unref(project);
}

static void test_defun_requires_parameter_list(void)
{
  Project *project = project_new(NULL);
  Document *document = project_add_document(project, g_string_new("(defun foo \"bad-params\" nil)"), NULL, DOCUMENT_LIVE);
  project_document_changed(project, document);

  const GArray *errors = document_get_errors(document);
  g_assert_nonnull(errors);
  g_assert_cmpuint(errors->len, ==, 1);
  const DocumentError *err = &g_array_index((GArray*)errors, DocumentError, 0);
  g_assert_nonnull(err->message);
  g_assert_cmpstr(err->message, ==, "DEFUN requires a parameter list.");
  g_assert_cmpint(err->type, ==, DOCUMENT_ERROR_TYPE_GENERIC);

  project_unref(project);
}

static void test_incremental_index(void)
{
  Project *project = project_new(NULL);
  Document *f1 = project_add_document(project, g_string_new("(defun foo () nil)"), NULL, DOCUMENT_LIVE);
  project_document_changed(project, f1);

  Document *f2 = project_add_document(project, g_string_new("(defun bar () nil)"), NULL, DOCUMENT_LIVE);
  project_document_changed(project, f2);

  GHashTable *defs = project_get_index(project, SDT_FUNCTION_DEF);
  g_assert_nonnull(g_hash_table_lookup(defs, "FOO"));
  g_assert_nonnull(g_hash_table_lookup(defs, "BAR"));

  document_set_text(f2, "(defun baz () nil)");
  project_document_changed(project, f2);

  g_assert_nonnull(g_hash_table_lookup(defs, "FOO"));
  g_assert_null(g_hash_table_lookup(defs, "BAR"));
  g_assert_nonnull(g_hash_table_lookup(defs, "BAZ"));

  g_assert_nonnull(project_get_function(project, "FOO"));
  g_assert_null(project_get_function(project, "BAR"));
  g_assert_nonnull(project_get_function(project, "BAZ"));

  project_unref(project);
}

static void test_function_call_argument_mismatch(void)
{
  Project *project = project_new(NULL);
  Function *fn = create_function_with_lambda("BAR", "(x y)");
  project_add_function(project, fn);
  function_unref(fn);

  Document *document = project_add_document(project, g_string_new("(bar 1)"), NULL,
      DOCUMENT_LIVE);
  project_document_changed(project, document);

  const GArray *errors = document_get_errors(document);
  g_assert_nonnull(errors);
  g_assert_cmpuint(errors->len, ==, 1);
  const DocumentError *err = &g_array_index((GArray*)errors,
      DocumentError, 0);
  g_assert_cmpuint(err->start, ==, 0);
  const GString *file_content = document_get_content(document);
  gsize len = file_content ? file_content->len : 0;
  g_assert_cmpuint(err->end, ==, len);
  g_assert_nonnull(err->message);
  g_assert_nonnull(project_get_function(project, "BAR"));
  g_assert_cmpstr(err->message, ==,
      "Expected 2 arguments for BAR but found 1");
  g_assert_cmpint(err->type, ==, DOCUMENT_ERROR_TYPE_GENERIC);

  document_set_text(document, "(bar 1 2)");
  project_document_changed(project, document);

  errors = document_get_errors(document);
  g_assert_nonnull(errors);
  g_assert_cmpuint(errors->len, ==, 0);

  document_set_text(document, "(bar 1 2 3)");
  project_document_changed(project, document);

  errors = document_get_errors(document);
  g_assert_nonnull(errors);
  g_assert_cmpuint(errors->len, ==, 1);
  err = &g_array_index((GArray*)errors, DocumentError, 0);
  g_assert_nonnull(err->message);
  g_assert_cmpstr(err->message, ==,
      "Expected 2 arguments for BAR but found 3");
  g_assert_cmpint(err->type, ==, DOCUMENT_ERROR_TYPE_GENERIC);

  document_set_text(document, "(bar 1 2)");
  project_document_changed(project, document);

  errors = document_get_errors(document);
  g_assert_nonnull(errors);
  g_assert_cmpuint(errors->len, ==, 0);

  project_unref(project);
}

static void test_undefined_function(void)
{
  Project *project = project_new(NULL);
  Document *document = project_add_document(project, g_string_new("(bar)"), NULL,
      DOCUMENT_LIVE);
  project_document_changed(project, document);

  const GArray *errors = document_get_errors(document);
  g_assert_nonnull(errors);
  g_assert_cmpuint(errors->len, ==, 1);
  const DocumentError *err = &g_array_index((GArray*)errors, DocumentError, 0);
  g_assert_cmpint(err->type, ==, DOCUMENT_ERROR_TYPE_UNDEFINED_FUNCTION);
  g_assert_cmpstr(err->message, ==, "Undefined function BAR");

  project_unref(project);
}

static void test_relative_path(void)
{
  gchar *tmpdir = g_dir_make_tmp("project-test-XXXXXX", NULL);
  Project *project = project_new(NULL);
  project_set_path(project, tmpdir);
  gchar *filepath = g_build_filename(tmpdir, "document.lisp", NULL);
  Document *document = project_add_document(project, g_string_new(""), filepath,
      DOCUMENT_LIVE);
  const gchar *rel = document_get_relative_path(document);
  g_assert_cmpstr(rel, ==, "document.lisp");
  project_unref(project);
  g_free(filepath);
  g_rmdir(tmpdir);
  g_free(tmpdir);
}

static void on_removed(Project * /*project*/, Document * /*document*/, gpointer user_data)
{
  gboolean *flag = user_data;
  *flag = TRUE;
}

static void test_remove_file(void)
{
  Project *project = project_new(NULL);
  Document *document = project_add_document(project, g_string_new(""), "foo.lisp",
      DOCUMENT_LIVE);
  guint before = project_get_document_count(project);
  gboolean removed = FALSE;
  project_set_document_removed_cb(project, on_removed, &removed);
  project_remove_document(project, document);
  g_assert_true(removed);
  g_assert_cmpuint(project_get_document_count(project), ==, before - 1);
  project_unref(project);
}

static void on_project_changed(Project * /*project*/, gpointer user_data)
{
  int *count = user_data;
  (*count)++;
}

static void test_project_changed_cb(void)
{
  Project *project = project_new(NULL);
  int count = 0;
  project_set_changed_cb(project, on_project_changed, &count);
  Package *pkg = package_new("FOO");
  project_add_package(project, pkg);
  package_unref(pkg);
  g_assert_cmpint(count, ==, 1);
  g_assert_nonnull(project_get_package(project, "FOO"));
  project_unref(project);
}


int main(int argc, char *argv[])
{
  g_test_init(&argc, &argv, NULL);
  GMainContext *ctx = g_main_context_default();
  g_main_context_push_thread_default(ctx);
  g_test_add_func("/project/default_file", test_default_file);
  g_test_add_func("/project/parse_on_change", test_parse_on_change);
  g_test_add_func("/project/file_load", test_file_load);
  g_test_add_func("/project/function_analysis", test_function_analysis);
  g_test_add_func("/project/index", test_index);
  g_test_add_func("/project/functions_table", test_functions_table);
  g_test_add_func("/function/tooltip", test_function_tooltip);
  g_test_add_func("/project/defun_requires_symbol_name",
      test_defun_requires_symbol_name);
  g_test_add_func("/project/defun_requires_parameter_list",
      test_defun_requires_parameter_list);
  g_test_add_func("/project/incremental_index", test_incremental_index);
  g_test_add_func("/project/function_call_argument_mismatch",
      test_function_call_argument_mismatch);
  g_test_add_func("/project/undefined_function", test_undefined_function);
  g_test_add_func("/project/relative_path", test_relative_path);
  g_test_add_func("/project/remove_file", test_remove_file);
  g_test_add_func("/project/project_changed_cb", test_project_changed_cb);
  int ret = g_test_run();
  g_main_context_pop_thread_default(ctx);
  return ret;
}
