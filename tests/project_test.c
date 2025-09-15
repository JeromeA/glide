#include "project.h"
#include "string_text_provider.h"
#include "node.h"
#include <glib.h>
#include <glib/gstdio.h>
#include <string.h>

static void test_default_file(void)
{
  Project *project = project_new(NULL);
  g_assert_cmpuint(project_get_file_count(project), ==, 1);
  ProjectFile *file = project_get_file(project, 0);
  g_assert_cmpint(project_file_get_state(file), ==, PROJECT_FILE_LIVE);
  g_assert_cmpstr(project_file_get_path(file), ==, "unnamed.lisp");
  project_unref(project);
}

static void test_parse_on_change(void)
{
  Project *project = project_new(NULL);
  TextProvider *provider = string_text_provider_new("(a)");
  ProjectFile *file = project_add_file(project, provider, NULL, NULL, PROJECT_FILE_LIVE);
  text_provider_unref(provider);
  project_file_changed(project, file);
  LispParser *parser = project_file_get_parser(file);
  LispLexer *lexer = project_file_get_lexer(file);
  GArray *tokens = lisp_lexer_get_tokens(lexer);
  g_assert_cmpint(tokens->len, ==, 3); /* (, a, ) */
  const LispToken *token = &g_array_index(tokens, LispToken, 0);
  g_assert_cmpint(token->type, ==, LISP_TOKEN_TYPE_LIST_START);
  project_file_changed(project, file); /* should still parse without error */
  const Node *ast = lisp_parser_get_ast(parser);
  g_assert_cmpint(ast->children->len, ==, 1);
  project_file_changed(project, file);
  project_unref(project);
}

static void on_file_loaded(Project * /*project*/, ProjectFile * /*file*/,
    gpointer user_data)
{
  int *count = user_data;
  (*count)++;
}

static void test_file_load(void)
{
  gchar *tmpdir = g_dir_make_tmp("project-test-XXXXXX", NULL);
  gchar *filepath = g_build_filename(tmpdir, "file.lisp", NULL);
  const gchar *contents = "(a b c)";
  g_file_set_contents(filepath, contents, -1, NULL);

  Project *project = project_new(NULL);
  TextProvider *provider = string_text_provider_new("");
  ProjectFile *file = project_add_file(project, provider, NULL, filepath,
      PROJECT_FILE_LIVE);
  text_provider_unref(provider);

  int count = 0;
  project_set_file_loaded_cb(project, on_file_loaded, &count);

  gboolean ok = project_file_load(file);
  g_assert_true(ok);
  g_assert_cmpint(count, ==, 1);

  TextProvider *tp = project_file_get_provider(file);
  gsize len = text_provider_get_length(tp);
  gchar *text = text_provider_get_text(tp, 0, len);
  g_assert_cmpstr(text, ==, contents);
  g_free(text);

  project_unref(project);
  g_remove(filepath);
  g_rmdir(tmpdir);
  g_free(filepath);
  g_free(tmpdir);
}

static void test_function_analysis(void)
{
  Project *project = project_new(NULL);
  TextProvider *provider = string_text_provider_new("(defun foo () (bar))");
  ProjectFile *file = project_add_file(project, provider, NULL, NULL, PROJECT_FILE_LIVE);
  text_provider_unref(provider);
  project_file_changed(project, file);
  LispParser *parser = project_file_get_parser(file);
  const Node *ast = lisp_parser_get_ast(parser);
  const Node *form = g_array_index(ast->children, Node*, 0);
  Node *defsym = g_array_index(form->children, Node*, 0);
  Node *name = g_array_index(form->children, Node*, 1);
  Node *call = g_array_index(form->children, Node*, 3);
  Node *callee = g_array_index(call->children, Node*, 0);
  g_assert_true(node_is(defsym, SDT_FUNCTION_USE));
  g_assert_true(node_is(name, SDT_FUNCTION_DEF));
  g_assert_true(node_is(callee, SDT_FUNCTION_USE));
  project_unref(project);
}

static void test_index(void)
{
  Project *project = project_new(NULL);
  TextProvider *provider = string_text_provider_new("(defun foo () (bar))");
  ProjectFile *file = project_add_file(project, provider, NULL, NULL,
      PROJECT_FILE_LIVE);
  text_provider_unref(provider);
  project_file_changed(project, file);
  LispParser *parser = project_file_get_parser(file);
  const Node *ast = lisp_parser_get_ast(parser);
  const Node *form = g_array_index(ast->children, Node*, 0);
  Node *defsym = g_array_index(form->children, Node*, 0);
  Node *name = g_array_index(form->children, Node*, 1);
  Node *call = g_array_index(form->children, Node*, 3);
  Node *callee = g_array_index(call->children, Node*, 0);
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
  TextProvider *provider = string_text_provider_new("(defun foo () \"doc\")");
  ProjectFile *file = project_add_file(project, provider, NULL, NULL, PROJECT_FILE_LIVE);
  text_provider_unref(provider);
  project_file_changed(project, file);
  Function *fn = project_get_function(project, "FOO");
  g_assert_nonnull(fn);
  const gchar *doc = function_get_doc_string(fn);
  g_assert_nonnull(doc);
  g_assert_cmpstr(doc, ==, "doc");
  g_assert_cmpstr(function_get_name(fn), ==, "FOO");
  g_assert_cmpstr(function_get_package(fn), ==, "CL-USER");
  project_unref(project);
}

static void test_function_tooltip(void)
{
  Project *project = project_new(NULL);
  TextProvider *provider = string_text_provider_new("(defun foo (x &rest rest) \"doc\")");
  ProjectFile *file = project_add_file(project, provider, NULL, NULL, PROJECT_FILE_LIVE);
  text_provider_unref(provider);
  project_file_changed(project, file);
  Function *fn = project_get_function(project, "FOO");
  gchar *tooltip = project_function_tooltip(fn);
  g_assert_nonnull(tooltip);
  g_assert_cmpstr(tooltip, ==,
      "In <span foreground=\"darkgreen\">CL-USER</span>:\n"
      "(<span foreground=\"brown\">FOO</span> X <span foreground=\"darkgreen\">&amp;REST</span> REST)\ndoc");
  g_free(tooltip);
  project_unref(project);
}

static void test_incremental_index(void)
{
  Project *project = project_new(NULL);
  TextProvider *p1 = string_text_provider_new("(defun foo () nil)");
  ProjectFile *f1 = project_add_file(project, p1, NULL, NULL, PROJECT_FILE_LIVE);
  text_provider_unref(p1);
  project_file_changed(project, f1);

  TextProvider *p2 = string_text_provider_new("(defun bar () nil)");
  ProjectFile *f2 = project_add_file(project, p2, NULL, NULL, PROJECT_FILE_LIVE);
  text_provider_unref(p2);
  project_file_changed(project, f2);

  GHashTable *defs = project_get_index(project, SDT_FUNCTION_DEF);
  g_assert_nonnull(g_hash_table_lookup(defs, "FOO"));
  g_assert_nonnull(g_hash_table_lookup(defs, "BAR"));

  StringTextProvider *stp2 = (StringTextProvider*)project_file_get_provider(f2);
  g_free(stp2->text);
  stp2->text = g_strdup("(defun baz () nil)");
  project_file_changed(project, f2);

  g_assert_nonnull(g_hash_table_lookup(defs, "FOO"));
  g_assert_null(g_hash_table_lookup(defs, "BAR"));
  g_assert_nonnull(g_hash_table_lookup(defs, "BAZ"));

  g_assert_nonnull(project_get_function(project, "FOO"));
  g_assert_null(project_get_function(project, "BAR"));
  g_assert_nonnull(project_get_function(project, "BAZ"));

  project_unref(project);
}

static void test_relative_path(void)
{
  gchar *tmpdir = g_dir_make_tmp("project-test-XXXXXX", NULL);
  Project *project = project_new(NULL);
  project_set_path(project, tmpdir);
  TextProvider *provider = string_text_provider_new("");
  gchar *filepath = g_build_filename(tmpdir, "file.lisp", NULL);
  ProjectFile *file = project_add_file(project, provider, NULL, filepath,
      PROJECT_FILE_LIVE);
  text_provider_unref(provider);
  const gchar *rel = project_file_get_relative_path(file);
  g_assert_cmpstr(rel, ==, "file.lisp");
  project_unref(project);
  g_free(filepath);
  g_rmdir(tmpdir);
  g_free(tmpdir);
}

static void on_removed(Project * /*project*/, ProjectFile * /*file*/, gpointer user_data)
{
  gboolean *flag = user_data;
  *flag = TRUE;
}

static void test_remove_file(void)
{
  Project *project = project_new(NULL);
  TextProvider *provider = string_text_provider_new("");
  ProjectFile *file = project_add_file(project, provider, NULL, "foo.lisp",
      PROJECT_FILE_LIVE);
  text_provider_unref(provider);
  guint before = project_get_file_count(project);
  gboolean removed = FALSE;
  project_set_file_removed_cb(project, on_removed, &removed);
  project_remove_file(project, file);
  g_assert_true(removed);
  g_assert_cmpuint(project_get_file_count(project), ==, before - 1);
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
  g_test_add_func("/project/function_tooltip", test_function_tooltip);
  g_test_add_func("/project/incremental_index", test_incremental_index);
  g_test_add_func("/project/relative_path", test_relative_path);
  g_test_add_func("/project/remove_file", test_remove_file);
  g_test_add_func("/project/project_changed_cb", test_project_changed_cb);
  int ret = g_test_run();
  g_main_context_pop_thread_default(ctx);
  return ret;
}
