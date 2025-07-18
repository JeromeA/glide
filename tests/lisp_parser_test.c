#include "lisp_parser.h"
#include "string_text_provider.h"
#include <glib.h>

static LispParser *parser_from_text(const gchar *text)
{
  TextProvider *provider = string_text_provider_new(text);
  LispParser *parser = lisp_parser_new(provider);
  lisp_parser_parse(parser);
  g_object_unref(provider);
  return parser;
}

static void test_empty_file(void)
{
  LispParser *parser = parser_from_text("");

  guint n_tokens = 0;
  lisp_parser_get_tokens(parser, &n_tokens);
  g_assert_cmpint(n_tokens, ==, 0);

  const LispAstNode *ast = lisp_parser_get_ast(parser);
  g_assert_cmpint(ast->children->len, ==, 0);

  lisp_parser_free(parser);
}

static void test_atom_symbol(void)
{
  LispParser *parser = parser_from_text("foo");

  guint n_tokens = 0;
  const LispToken *tokens = lisp_parser_get_tokens(parser, &n_tokens);
  g_assert_cmpint(n_tokens, ==, 1);
  g_assert_cmpint(tokens[0].type, ==, LISP_TOKEN_TYPE_ATOM);

  const LispAstNode *ast = lisp_parser_get_ast(parser);
  g_assert_cmpint(ast->children->len, ==, 1);
  const LispAstNode *child = g_array_index(ast->children, LispAstNode*, 0);
  g_assert_cmpint(child->type, ==, LISP_AST_NODE_TYPE_ATOM);
  g_assert_cmpstr(child->start_token->text, ==, "foo");

  lisp_parser_free(parser);
}

static void test_atom_string(void)
{
  LispParser *parser = parser_from_text("\"bar\"");

  guint n_tokens = 0;
  const LispToken *tokens = lisp_parser_get_tokens(parser, &n_tokens);
  g_assert_cmpint(n_tokens, ==, 1);
  g_assert_cmpint(tokens[0].type, ==, LISP_TOKEN_TYPE_STRING);

  const LispAstNode *ast = lisp_parser_get_ast(parser);
  g_assert_cmpint(ast->children->len, ==, 1);
  const LispAstNode *child = g_array_index(ast->children, LispAstNode*, 0);
  g_assert_cmpint(child->type, ==, LISP_AST_NODE_TYPE_STRING);
  g_assert_cmpstr(child->start_token->text, ==, "\"bar\"");

  lisp_parser_free(parser);
}

static void test_empty_list(void)
{
  LispParser *parser = parser_from_text("()");

  const LispAstNode *ast = lisp_parser_get_ast(parser);
  g_assert_cmpint(ast->children->len, ==, 1);

  const LispAstNode *child = g_array_index(ast->children, LispAstNode*, 0);
  g_assert_cmpint(child->type, ==, LISP_AST_NODE_TYPE_LIST);
  g_assert_cmpint(child->children->len, ==, 0);

  lisp_parser_free(parser);
}

static void test_list_with_elements(void)
{
  LispParser *parser = parser_from_text("(a b)");

  const LispAstNode *ast = lisp_parser_get_ast(parser);
  g_assert_cmpint(ast->children->len, ==, 1);

  const LispAstNode *list = g_array_index(ast->children, LispAstNode*, 0);
  g_assert_cmpint(list->type, ==, LISP_AST_NODE_TYPE_LIST);
  g_assert_cmpint(list->children->len, ==, 2);

  const LispAstNode *a = g_array_index(list->children, LispAstNode*, 0);
  const LispAstNode *b = g_array_index(list->children, LispAstNode*, 1);
  g_assert_cmpint(a->type, ==, LISP_AST_NODE_TYPE_ATOM);
  g_assert_cmpstr(a->start_token->text, ==, "a");
  g_assert_cmpint(b->type, ==, LISP_AST_NODE_TYPE_ATOM);
  g_assert_cmpstr(b->start_token->text, ==, "b");

  lisp_parser_free(parser);
}

static void test_missing_closing_paren(void)
{
  LispParser *parser = parser_from_text("(a b");

  const LispAstNode *ast = lisp_parser_get_ast(parser);
  g_assert_cmpint(ast->children->len, ==, 1);

  const LispAstNode *list = g_array_index(ast->children, LispAstNode*, 0);
  g_assert_cmpint(list->type, ==, LISP_AST_NODE_TYPE_LIST);
  g_assert_cmpint(list->children->len, ==, 2);

  const LispAstNode *a = g_array_index(list->children, LispAstNode*, 0);
  const LispAstNode *b = g_array_index(list->children, LispAstNode*, 1);
  g_assert_cmpint(a->type, ==, LISP_AST_NODE_TYPE_ATOM);
  g_assert_cmpstr(a->start_token->text, ==, "a");
  g_assert_cmpint(b->type, ==, LISP_AST_NODE_TYPE_ATOM);
  g_assert_cmpstr(b->start_token->text, ==, "b");

  lisp_parser_free(parser);
}

static void test_extra_closing_paren(void)
{
  LispParser *parser = parser_from_text(")");

  guint n_tokens = 0;
  const LispToken *tokens = lisp_parser_get_tokens(parser, &n_tokens);
  g_assert_cmpint(n_tokens, ==, 1);
  g_assert_cmpint(tokens[0].type, ==, LISP_TOKEN_TYPE_LIST_END);

  const LispAstNode *ast = lisp_parser_get_ast(parser);
  g_assert_cmpint(ast->children->len, ==, 0);

  lisp_parser_free(parser);
}

static void test_comment(void)
{
  LispParser *parser = parser_from_text("; a comment\n");

  guint n_tokens = 0;
  const LispToken *tokens = lisp_parser_get_tokens(parser, &n_tokens);
  g_assert_cmpint(n_tokens, ==, 2);
  g_assert_cmpint(tokens[0].type, ==, LISP_TOKEN_TYPE_COMMENT);

  const LispAstNode *ast = lisp_parser_get_ast(parser);
  g_assert_cmpint(ast->children->len, ==, 0);

  lisp_parser_free(parser);
}

int main(int argc, char *argv[])
{
  g_test_init(&argc, &argv, NULL);
  g_test_add_func("/lisp_parser/empty_file", test_empty_file);
  g_test_add_func("/lisp_parser/atom_symbol", test_atom_symbol);
  g_test_add_func("/lisp_parser/atom_string", test_atom_string);
  g_test_add_func("/lisp_parser/empty_list", test_empty_list);
  g_test_add_func("/lisp_parser/list_with_elements", test_list_with_elements);
  g_test_add_func("/lisp_parser/missing_closing_paren", test_missing_closing_paren);
  g_test_add_func("/lisp_parser/extra_closing_paren", test_extra_closing_paren);
  g_test_add_func("/lisp_parser/comment", test_comment);
  return g_test_run();
}

