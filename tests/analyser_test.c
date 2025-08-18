#include "analyser.h"
#include "lisp_lexer.h"
#include "lisp_parser.h"
#include "node.h"
#include "string_text_provider.h"
#include <glib.h>

int main(void) {
  const gchar *text =
    "(defun foo ())\n"
    "(in-package \"MY-PACK\")\n"
    "(defun bar ())";
  TextProvider *provider = string_text_provider_new(text);
  LispLexer *lexer = lisp_lexer_new(provider);
  lisp_lexer_lex(lexer);
  LispParser *parser = lisp_parser_new();
  GArray *tokens = lisp_lexer_get_tokens(lexer);
  lisp_parser_parse(parser, tokens);
  Node *ast = (Node*)lisp_parser_get_ast(parser);

  analyse_ast(ast);

  Node *foo_expr = g_array_index(ast->children, Node*, 0);
  Node *foo_name = g_array_index(foo_expr->children, Node*, 1);
  g_assert_cmpstr(foo_name->package_context, ==, "COMMON-LISP-USER");

  Node *bar_expr = g_array_index(ast->children, Node*, 2);
  Node *bar_name = g_array_index(bar_expr->children, Node*, 1);
  g_assert_cmpstr(bar_name->package_context, ==, "MY-PACK");

  lisp_parser_free(parser);
  lisp_lexer_free(lexer);
  g_object_unref(provider);
  return 0;
}
