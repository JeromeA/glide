#include "analyse.h"
#include "lisp_lexer.h"
#include "lisp_parser.h"
#include "node.h"
#include "project.h"
#include <glib.h>

typedef struct {
  GArray *tokens;
  Node *ast;
} ParseFixture;

static ParseFixture parse_fixture_new(const gchar *text) {
  ParseFixture fixture;
  GString *content = g_string_new(text);
  fixture.tokens = lisp_lexer_lex(content);
  fixture.ast = lisp_parser_parse(fixture.tokens, NULL);
  g_string_free(content, TRUE);
  return fixture;
}

static void parse_fixture_free(ParseFixture *fixture) {
  if (fixture->ast)
    node_free_deep(fixture->ast);
  if (fixture->tokens)
    g_array_free(fixture->tokens, TRUE);
}

static void test_analyse(void) {
  const gchar *text =
    "(defpackage :my-pack (:nicknames :mp) (:use :cl))\n"
    "(defun foo ())\n"
    "(in-package :my-pack)\n"
    "(defun bar ())\n"
    "(foo baz)";
  ParseFixture fixture = parse_fixture_new(text);
  Node *ast = fixture.ast;

  Project *project = project_new(NULL);
  analyse_ast(project, ast);

  Package *pkg = project_get_package(project, "MY-PACK");
  g_assert_nonnull(pkg);
  g_assert_true(g_hash_table_contains(package_get_nicknames(pkg), "MP"));
  g_assert_true(g_hash_table_contains(package_get_uses(pkg), "CL"));

  Node *foo_expr = g_array_index(ast->children, Node*, 1);
  Node *foo_symbol = g_array_index(foo_expr->children, Node*, 1);
  Node *foo_name = node_get_symbol_name_node(foo_symbol);
  g_assert_nonnull(foo_name);
  g_assert_cmpstr(foo_name->package_context, ==, "CL-USER");

  Node *bar_expr = g_array_index(ast->children, Node*, 3);
  Node *bar_symbol = g_array_index(bar_expr->children, Node*, 1);
  Node *bar_name = node_get_symbol_name_node(bar_symbol);
  g_assert_nonnull(bar_name);
  g_assert_cmpstr(bar_name->package_context, ==, "MY-PACK");

  Node *call_expr = g_array_index(ast->children, Node*, 4);
  Node *var_symbol = g_array_index(call_expr->children, Node*, 1);
  Node *var_use = node_get_symbol_name_node(var_symbol);
  g_assert_nonnull(var_use);
  g_assert(node_is(var_use, SDT_VAR_USE));
  g_assert_cmpstr(var_use->package_context, ==, "MY-PACK");

  parse_fixture_free(&fixture);
  project_unref(project);
}

static void test_nested_defpackage(void) {
  const gchar *text =
    "(defpackage :top)\n"
    "(let () (defpackage :inner))";
  ParseFixture fixture = parse_fixture_new(text);
  Node *ast = fixture.ast;

  Project *project = project_new(NULL);
  analyse_ast(project, ast);

  g_assert_nonnull(project_get_package(project, "TOP"));
  g_assert_null(project_get_package(project, "INNER"));

  Node *let_expr = g_array_index(ast->children, Node*, 1);
  Node *inner_expr = g_array_index(let_expr->children, Node*, 2);
  Node *inner_name = g_array_index(inner_expr->children, Node*, 1);
  g_assert(node_is(inner_name, SDT_PACKAGE_DEF));

  parse_fixture_free(&fixture);
  project_unref(project);
}

static void test_nested_defun(void) {
  const gchar *text =
    "(defun top ())\n"
    "(let () (defun inner ()))";
  ParseFixture fixture = parse_fixture_new(text);
  Node *ast = fixture.ast;

  Project *project = project_new(NULL);
  analyse_ast(project, ast);

  g_assert_nonnull(project_get_function(project, "TOP"));
  g_assert_null(project_get_function(project, "INNER"));

  Node *let_expr = g_array_index(ast->children, Node*, 1);
  Node *inner_expr = g_array_index(let_expr->children, Node*, 2);
  Node *inner_symbol = g_array_index(inner_expr->children, Node*, 1);
  Node *inner_name = node_get_symbol_name_node(inner_symbol);
  g_assert_nonnull(inner_name);
  g_assert(node_is(inner_name, SDT_FUNCTION_DEF));

  parse_fixture_free(&fixture);
  project_unref(project);
}

static void test_backquote_defpackage(void) {
  const gchar *text =
    "`(defpackage foo (:export ,(progn (defun inner ()) '(inner))))";
  ParseFixture fixture = parse_fixture_new(text);
  Node *ast = fixture.ast;

  Project *project = project_new(NULL);
  analyse_ast(project, ast);

  g_assert_null(project_get_package(project, "FOO"));

  Node *backquote = g_array_index(ast->children, Node*, 0);
  Node *defpackage = g_array_index(backquote->children, Node*, 0);
  Node *option = g_array_index(defpackage->children, Node*, 2);
  Node *unquote = g_array_index(option->children, Node*, 1);
  Node *progn = g_array_index(unquote->children, Node*, 0);
  Node *defun = g_array_index(progn->children, Node*, 1);
  Node *inner_symbol = g_array_index(defun->children, Node*, 1);
  Node *inner_name = node_get_symbol_name_node(inner_symbol);
  g_assert_nonnull(inner_name);
  g_assert(node_is(inner_name, SDT_FUNCTION_DEF));

  parse_fixture_free(&fixture);
  project_unref(project);
}

int main(int argc, char *argv[]) {
  g_test_init(&argc, &argv, NULL);
  GMainContext *ctx = g_main_context_default();
  g_main_context_push_thread_default(ctx);
  g_test_add_func("/analyse/basic", test_analyse);
  g_test_add_func("/analyse/nested-defpackage", test_nested_defpackage);
  g_test_add_func("/analyse/nested-defun", test_nested_defun);
  g_test_add_func("/analyse/backquote-defpackage", test_backquote_defpackage);
  int ret = g_test_run();
  g_main_context_pop_thread_default(ctx);
  return ret;
}
