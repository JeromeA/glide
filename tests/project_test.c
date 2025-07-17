#include "project.h"
#include "string_text_provider.h"
#include <glib.h>
#include <glib/gstdio.h>
#include <string.h>

static void test_default_scratch(void)
{
  Project *project = project_new();
  g_assert_cmpuint(project_get_file_count(project), ==, 1);
  ProjectFile *file = project_get_file(project, 0);
  g_assert_cmpint(project_file_get_state(file), ==, PROJECT_FILE_SCRATCH);
  g_assert_cmpstr(project_file_get_path(file), ==, "scratch00");
  g_object_unref(project);
}

static void test_parse_on_change(void)
{
  Project *project = project_new();
  TextProvider *provider = string_text_provider_new("(a)");
  ProjectFile *file = project_add_file(project, provider, NULL, NULL, PROJECT_FILE_SCRATCH);
  g_object_unref(provider);
  /* file already parsed by project_add_file */
  LispParser *parser = project_file_get_parser(file);
  guint n_tokens = 0;
  const LispToken *tokens = lisp_parser_get_tokens(parser, &n_tokens);
  g_assert_cmpint(n_tokens, ==, 3); /* (, a, ) */
  g_assert_cmpint(tokens[0].type, ==, LISP_TOKEN_TYPE_LIST_START);
  project_file_changed(project, file); /* should still parse without error */
  const LispAstNode *ast = lisp_parser_get_ast(parser);
  g_assert_cmpint(ast->children->len, ==, 1);
  project_file_changed(project, file);
  g_object_unref(project);
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

  Project *project = project_new();
  TextProvider *provider = string_text_provider_new("");
  ProjectFile *file = project_add_file(project, provider, NULL, filepath,
      PROJECT_FILE_LIVE);
  g_object_unref(provider);

  int count = 0;
  g_signal_connect(project, "file-loaded", G_CALLBACK(on_file_loaded), &count);

  gboolean ok = project_file_load(project, file);
  g_assert_true(ok);
  g_assert_cmpint(count, ==, 1);

  TextProvider *tp = project_file_get_provider(file);
  gsize len = text_provider_get_length(tp);
  gchar *text = text_provider_get_text(tp, 0, len);
  g_assert_cmpstr(text, ==, contents);
  g_free(text);

  g_object_unref(project);
  g_remove(filepath);
  g_rmdir(tmpdir);
  g_free(filepath);
  g_free(tmpdir);
}

int main(int argc, char *argv[])
{
  g_test_init(&argc, &argv, NULL);
  g_test_add_func("/project/default_scratch", test_default_scratch);
  g_test_add_func("/project/parse_on_change", test_parse_on_change);
  g_test_add_func("/project/file_load", test_file_load);
  return g_test_run();
}
