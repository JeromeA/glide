#include "project.h"
#include "string_text_provider.h"
#include <glib.h>

static void test_default_scratch(void)
{
  Project *project = project_new();
  g_assert_cmpuint(project_get_file_count(project), ==, 1);
  ProjectFile *file = project_get_file(project, 0);
  g_assert_cmpint(project_file_get_state(file), ==, PROJECT_FILE_SCRATCH);
  g_assert_cmpstr(project_file_get_path(file), ==, "scratch00");
  g_assert_true(project_get_current_file(project) == file);
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

static void test_set_current(void)
{
  Project *project = project_new();
  TextProvider *provider = string_text_provider_new("(b)");
  ProjectFile *file = project_add_file(project, provider, NULL, NULL,
      PROJECT_FILE_SCRATCH);
  g_object_unref(provider);
  project_set_current_file(project, file);
  g_assert_true(project_get_current_file(project) == file);
  g_object_unref(project);
}

int main(int argc, char *argv[])
{
  g_test_init(&argc, &argv, NULL);
  g_test_add_func("/project/default_scratch", test_default_scratch);
  g_test_add_func("/project/parse_on_change", test_parse_on_change);
  g_test_add_func("/project/set_current", test_set_current);
  return g_test_run();
}
