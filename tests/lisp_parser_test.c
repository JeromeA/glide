#include "lisp_lexer.h"
#include "lisp_parser.h"
#include "string_text_provider.h"
#include <glib.h>

typedef struct {
  LispLexer *lexer;
  LispParser *parser;
} ParserFixture;

static ParserFixture parser_fixture_from_text(const gchar *text) {
  ParserFixture fixture;
  TextProvider *provider = string_text_provider_new(text);
  fixture.lexer = lisp_lexer_new(provider);
  lisp_lexer_lex(fixture.lexer);
  fixture.parser = lisp_parser_new();
  guint n_tokens = 0;
  const LispToken *tokens = lisp_lexer_get_tokens(fixture.lexer, &n_tokens);
  lisp_parser_parse(fixture.parser, tokens, n_tokens);
  g_object_unref(provider);
  return fixture;
}

static void parser_fixture_free(ParserFixture *fixture) {
  lisp_parser_free(fixture->parser);
  lisp_lexer_free(fixture->lexer);
}

static void test_empty_file(void) {
  ParserFixture fixture = parser_fixture_from_text("");

  guint n_tokens = 0;
  lisp_lexer_get_tokens(fixture.lexer, &n_tokens);
  g_assert_cmpint(n_tokens, ==, 0);

  const LispAstNode *ast = lisp_parser_get_ast(fixture.parser);
  g_assert_cmpint(ast->children->len, ==, 0);

  parser_fixture_free(&fixture);
}

static void test_symbol(void) {
  ParserFixture fixture = parser_fixture_from_text("foo");

  guint n_tokens = 0;
  const LispToken *tokens = lisp_lexer_get_tokens(fixture.lexer, &n_tokens);
  g_assert_cmpint(n_tokens, ==, 1);
  g_assert_cmpint(tokens[0].type, ==, LISP_TOKEN_TYPE_SYMBOL);

  const LispAstNode *ast = lisp_parser_get_ast(fixture.parser);
  g_assert_cmpint(ast->children->len, ==, 1);
  const LispAstNode *child = g_array_index(ast->children, LispAstNode*, 0);
  g_assert_cmpint(child->type, ==, LISP_AST_NODE_TYPE_SYMBOL);
  g_assert_cmpstr(child->start_token->text, ==, "foo");

  parser_fixture_free(&fixture);
}

static void test_number(void) {
  ParserFixture fixture = parser_fixture_from_text("42");

  guint n_tokens = 0;
  const LispToken *tokens = lisp_lexer_get_tokens(fixture.lexer, &n_tokens);
  g_assert_cmpint(n_tokens, ==, 1);
  g_assert_cmpint(tokens[0].type, ==, LISP_TOKEN_TYPE_NUMBER);

  const LispAstNode *ast = lisp_parser_get_ast(fixture.parser);
  g_assert_cmpint(ast->children->len, ==, 1);
  const LispAstNode *child = g_array_index(ast->children, LispAstNode*, 0);
  g_assert_cmpint(child->type, ==, LISP_AST_NODE_TYPE_NUMBER);
  g_assert_cmpstr(child->start_token->text, ==, "42");

  parser_fixture_free(&fixture);
}

static void test_atom_string(void) {
  ParserFixture fixture = parser_fixture_from_text("\"bar\"");

  guint n_tokens = 0;
  const LispToken *tokens = lisp_lexer_get_tokens(fixture.lexer, &n_tokens);
  g_assert_cmpint(n_tokens, ==, 1);
  g_assert_cmpint(tokens[0].type, ==, LISP_TOKEN_TYPE_STRING);

  const LispAstNode *ast = lisp_parser_get_ast(fixture.parser);
  g_assert_cmpint(ast->children->len, ==, 1);
  const LispAstNode *child = g_array_index(ast->children, LispAstNode*, 0);
  g_assert_cmpint(child->type, ==, LISP_AST_NODE_TYPE_STRING);
  g_assert_cmpstr(child->start_token->text, ==, "\"bar\"");

  parser_fixture_free(&fixture);
}

static void test_empty_list(void) {
  ParserFixture fixture = parser_fixture_from_text("()");

  const LispAstNode *ast = lisp_parser_get_ast(fixture.parser);
  g_assert_cmpint(ast->children->len, ==, 1);

  const LispAstNode *child = g_array_index(ast->children, LispAstNode*, 0);
  g_assert_cmpint(child->type, ==, LISP_AST_NODE_TYPE_LIST);
  g_assert_cmpint(child->children->len, ==, 0);

  parser_fixture_free(&fixture);
}

static void test_list_with_elements(void) {
  ParserFixture fixture = parser_fixture_from_text("(a b)");

  const LispAstNode *ast = lisp_parser_get_ast(fixture.parser);
  g_assert_cmpint(ast->children->len, ==, 1);

  const LispAstNode *list = g_array_index(ast->children, LispAstNode*, 0);
  g_assert_cmpint(list->type, ==, LISP_AST_NODE_TYPE_LIST);
  g_assert_cmpint(list->children->len, ==, 2);

  const LispAstNode *a = g_array_index(list->children, LispAstNode*, 0);
  const LispAstNode *b = g_array_index(list->children, LispAstNode*, 1);
  g_assert_cmpint(a->type, ==, LISP_AST_NODE_TYPE_SYMBOL);
  g_assert_cmpstr(a->start_token->text, ==, "a");
  g_assert_cmpint(b->type, ==, LISP_AST_NODE_TYPE_SYMBOL);
  g_assert_cmpstr(b->start_token->text, ==, "b");

  parser_fixture_free(&fixture);
}

static void test_missing_closing_paren(void) {
  ParserFixture fixture = parser_fixture_from_text("(a b");

  const LispAstNode *ast = lisp_parser_get_ast(fixture.parser);
  g_assert_cmpint(ast->children->len, ==, 1);

  const LispAstNode *list = g_array_index(ast->children, LispAstNode*, 0);
  g_assert_cmpint(list->type, ==, LISP_AST_NODE_TYPE_LIST);
  g_assert_cmpint(list->children->len, ==, 2);

  const LispAstNode *a = g_array_index(list->children, LispAstNode*, 0);
  const LispAstNode *b = g_array_index(list->children, LispAstNode*, 1);
  g_assert_cmpint(a->type, ==, LISP_AST_NODE_TYPE_SYMBOL);
  g_assert_cmpstr(a->start_token->text, ==, "a");
  g_assert_cmpint(b->type, ==, LISP_AST_NODE_TYPE_SYMBOL);
  g_assert_cmpstr(b->start_token->text, ==, "b");

  parser_fixture_free(&fixture);
}

static void test_extra_closing_paren(void) {
  ParserFixture fixture = parser_fixture_from_text(")");

  guint n_tokens = 0;
  const LispToken *tokens = lisp_lexer_get_tokens(fixture.lexer, &n_tokens);
  g_assert_cmpint(n_tokens, ==, 1);
  g_assert_cmpint(tokens[0].type, ==, LISP_TOKEN_TYPE_LIST_END);

  const LispAstNode *ast = lisp_parser_get_ast(fixture.parser);
  g_assert_cmpint(ast->children->len, ==, 0);

  parser_fixture_free(&fixture);
}

static void test_comment(void) {
  ParserFixture fixture = parser_fixture_from_text("; a comment\n");

  guint n_tokens = 0;
  const LispToken *tokens = lisp_lexer_get_tokens(fixture.lexer, &n_tokens);
  g_assert_cmpint(n_tokens, ==, 2);
  g_assert_cmpint(tokens[0].type, ==, LISP_TOKEN_TYPE_COMMENT);

  const LispAstNode *ast = lisp_parser_get_ast(fixture.parser);
  g_assert_cmpint(ast->children->len, ==, 0);

  parser_fixture_free(&fixture);
}

int main(int argc, char *argv[]) {
  g_test_init(&argc, &argv, NULL);
  g_test_add_func("/lisp_parser/empty_file", test_empty_file);
  g_test_add_func("/lisp_parser/symbol", test_symbol);
  g_test_add_func("/lisp_parser/number", test_number);
  g_test_add_func("/lisp_parser/atom_string", test_atom_string);
  g_test_add_func("/lisp_parser/empty_list", test_empty_list);
  g_test_add_func("/lisp_parser/list_with_elements", test_list_with_elements);
  g_test_add_func("/lisp_parser/missing_closing_paren", test_missing_closing_paren);
  g_test_add_func("/lisp_parser/extra_closing_paren", test_extra_closing_paren);
  g_test_add_func("/lisp_parser/comment", test_comment);
  return g_test_run();
}

