#include "analyse.h"
#include "lisp_lexer.h"
#include "lisp_parser.h"
#include "node.h"
#include "string_text_provider.h"
#include "project.h"
#include <glib.h>

int main(void) {
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
  lisp_parser_parse(parser, tokens);
  Node *ast = (Node*)lisp_parser_get_ast(parser);

  Project *project = project_new();
  analyse_ast(project, ast);

  Package *pkg = project_get_package(project, "MY-PACK");
  g_assert(pkg);
  g_assert(g_hash_table_contains(package_get_nicknames(pkg), "MP"));
  g_assert(g_hash_table_contains(package_get_uses(pkg), "CL"));

  Node *foo_expr = g_array_index(ast->children, Node*, 1);
  Node *foo_name = g_array_index(foo_expr->children, Node*, 1);
  g_assert_cmpstr(foo_name->package_context, ==, "COMMON-LISP-USER");

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
  return 0;
}
