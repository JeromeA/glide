#include "analyse.h"
#include "lisp_lexer.h"
#include "lisp_parser.h"
#include "node.h"
#include "string_text_provider.h"
#include "project.h"
#include <glib.h>

static void test_analyse(void) {
  const gchar *text =
    "(defpackage :my-pack (:nicknames :mp) (:use :cl))\n"
    "(defun foo ())\n"
    "(in-package :my-pack)\n"
    "(defun bar ())\n"
    "(foo baz)";
  TextProvider *provider = string_text_provider_new(text);
  LispLexer *lexer = lisp_lexer_new(provider);
  lisp_lexer_lex(lexer);
  LispParser *parser = lisp_parser_new();
  GArray *tokens = lisp_lexer_get_tokens(lexer);
  lisp_parser_parse(parser, tokens, NULL);
  Node *ast = (Node*)lisp_parser_get_ast(parser);

  Project *project = project_new(NULL);
  analyse_ast(project, ast);

  Package *pkg = project_get_package(project, "MY-PACK");
  g_assert_nonnull(pkg);
  g_assert_true(g_hash_table_contains(package_get_nicknames(pkg), "MP"));
  g_assert_true(g_hash_table_contains(package_get_uses(pkg), "CL"));

  Node *foo_expr = g_array_index(ast->children, Node*, 1);
  Node *foo_name = g_array_index(foo_expr->children, Node*, 1);
  g_assert_cmpstr(foo_name->package_context, ==, "CL-USER");

  Node *bar_expr = g_array_index(ast->children, Node*, 3);
  Node *bar_name = g_array_index(bar_expr->children, Node*, 1);
  g_assert_cmpstr(bar_name->package_context, ==, "MY-PACK");

  Node *call_expr = g_array_index(ast->children, Node*, 4);
  Node *var_use = g_array_index(call_expr->children, Node*, 1);
  g_assert(node_is(var_use, SDT_VAR_USE));
  g_assert_cmpstr(var_use->package_context, ==, "MY-PACK");

  lisp_parser_free(parser);
  lisp_lexer_free(lexer);
  text_provider_unref(provider);
  project_unref(project);
}

static void test_nested_defpackage(void) {
  const gchar *text =
    "(defpackage :top)\n"
    "(let () (defpackage :inner))";
  TextProvider *provider = string_text_provider_new(text);
  LispLexer *lexer = lisp_lexer_new(provider);
  lisp_lexer_lex(lexer);
  LispParser *parser = lisp_parser_new();
  GArray *tokens = lisp_lexer_get_tokens(lexer);
  lisp_parser_parse(parser, tokens, NULL);
  Node *ast = (Node*)lisp_parser_get_ast(parser);

  Project *project = project_new(NULL);
  analyse_ast(project, ast);

  g_assert_nonnull(project_get_package(project, "TOP"));
  g_assert_null(project_get_package(project, "INNER"));

  Node *let_expr = g_array_index(ast->children, Node*, 1);
  Node *inner_expr = g_array_index(let_expr->children, Node*, 2);
  Node *inner_name = g_array_index(inner_expr->children, Node*, 1);
  g_assert(node_is(inner_name, SDT_PACKAGE_DEF));

  lisp_parser_free(parser);
  lisp_lexer_free(lexer);
  text_provider_unref(provider);
  project_unref(project);
}

static void test_nested_defun(void) {
  const gchar *text =
    "(defun top ())\n"
    "(let () (defun inner ()))";
  TextProvider *provider = string_text_provider_new(text);
  LispLexer *lexer = lisp_lexer_new(provider);
  lisp_lexer_lex(lexer);
  LispParser *parser = lisp_parser_new();
  GArray *tokens = lisp_lexer_get_tokens(lexer);
  lisp_parser_parse(parser, tokens, NULL);
  Node *ast = (Node*)lisp_parser_get_ast(parser);

  Project *project = project_new(NULL);
  analyse_ast(project, ast);

  g_assert_nonnull(project_get_function(project, "TOP"));
  g_assert_null(project_get_function(project, "INNER"));

  Node *let_expr = g_array_index(ast->children, Node*, 1);
  Node *inner_expr = g_array_index(let_expr->children, Node*, 2);
  Node *inner_name = g_array_index(inner_expr->children, Node*, 1);
  g_assert(node_is(inner_name, SDT_FUNCTION_DEF));

  lisp_parser_free(parser);
  lisp_lexer_free(lexer);
  text_provider_unref(provider);
  project_unref(project);
}

int main(int argc, char *argv[]) {
  g_test_init(&argc, &argv, NULL);
  g_test_add_func("/analyse/basic", test_analyse);
  g_test_add_func("/analyse/nested-defpackage", test_nested_defpackage);
  g_test_add_func("/analyse/nested-defun", test_nested_defun);
  return g_test_run();
}
