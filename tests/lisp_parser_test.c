#include "lisp_lexer.h"
#include "lisp_parser.h"
#include "string_text_provider.h"
#include "node.h"
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
  GArray *tokens = lisp_lexer_get_tokens(fixture.lexer);
  lisp_parser_parse(fixture.parser, tokens, NULL);
  text_provider_unref(provider);
  return fixture;
}

static void parser_fixture_free(ParserFixture *fixture) {
  lisp_parser_free(fixture->parser);
  lisp_lexer_free(fixture->lexer);
}

static void test_empty_file(void) {
  ParserFixture fixture = parser_fixture_from_text("");

  GArray *tokens = lisp_lexer_get_tokens(fixture.lexer);
  g_assert_cmpint(tokens->len, ==, 0);

  const Node *ast = lisp_parser_get_ast(fixture.parser);
  g_assert_cmpint(ast->children->len, ==, 0);

  parser_fixture_free(&fixture);
}

static void test_symbol(void) {
  ParserFixture fixture = parser_fixture_from_text("foo");

  GArray *tokens = lisp_lexer_get_tokens(fixture.lexer);
  g_assert_cmpint(tokens->len, ==, 1);
  const LispToken *token = &g_array_index(tokens, LispToken, 0);
  g_assert_cmpint(token->type, ==, LISP_TOKEN_TYPE_SYMBOL);

  const Node *ast = lisp_parser_get_ast(fixture.parser);
  g_assert_cmpint(ast->children->len, ==, 1);
  const Node *child = g_array_index(ast->children, Node*, 0);
  g_assert_cmpint(child->type, ==, LISP_AST_NODE_TYPE_SYMBOL);
  g_assert_cmpint(child->children->len, ==, 1);
  const Node *name = g_array_index(child->children, Node*, 0);
  g_assert_cmpint(name->type, ==, LISP_AST_NODE_TYPE_SYMBOL_NAME);
  g_assert_cmpstr(name->start_token->text, ==, "foo");

  parser_fixture_free(&fixture);
}

static void test_number(void) {
  ParserFixture fixture = parser_fixture_from_text("42");

  GArray *tokens = lisp_lexer_get_tokens(fixture.lexer);
  g_assert_cmpint(tokens->len, ==, 1);
  const LispToken *token = &g_array_index(tokens, LispToken, 0);
  g_assert_cmpint(token->type, ==, LISP_TOKEN_TYPE_NUMBER);

  const Node *ast = lisp_parser_get_ast(fixture.parser);
  g_assert_cmpint(ast->children->len, ==, 1);
  const Node *child = g_array_index(ast->children, Node*, 0);
  g_assert_cmpint(child->type, ==, LISP_AST_NODE_TYPE_NUMBER);
  g_assert_cmpstr(child->start_token->text, ==, "42");

  parser_fixture_free(&fixture);
}

static void test_atom_string(void) {
  ParserFixture fixture = parser_fixture_from_text("\"bar\"");

  GArray *tokens = lisp_lexer_get_tokens(fixture.lexer);
  g_assert_cmpint(tokens->len, ==, 1);
  const LispToken *token = &g_array_index(tokens, LispToken, 0);
  g_assert_cmpint(token->type, ==, LISP_TOKEN_TYPE_STRING);

  const Node *ast = lisp_parser_get_ast(fixture.parser);
  g_assert_cmpint(ast->children->len, ==, 1);
  const Node *child = g_array_index(ast->children, Node*, 0);
  g_assert_cmpint(child->type, ==, LISP_AST_NODE_TYPE_STRING);
  g_assert_cmpstr(child->start_token->text, ==, "\"bar\"");

  parser_fixture_free(&fixture);
}

static void test_empty_list(void) {
  ParserFixture fixture = parser_fixture_from_text("()");

  const Node *ast = lisp_parser_get_ast(fixture.parser);
  g_assert_cmpint(ast->children->len, ==, 1);

  const Node *child = g_array_index(ast->children, Node*, 0);
  g_assert_cmpint(child->type, ==, LISP_AST_NODE_TYPE_LIST);
  g_assert_cmpint(child->children->len, ==, 0);

  parser_fixture_free(&fixture);
}

static void test_list_with_elements(void) {
  ParserFixture fixture = parser_fixture_from_text("(a b)");

  const Node *ast = lisp_parser_get_ast(fixture.parser);
  g_assert_cmpint(ast->children->len, ==, 1);

  const Node *list = g_array_index(ast->children, Node*, 0);
  g_assert_cmpint(list->type, ==, LISP_AST_NODE_TYPE_LIST);
  g_assert_cmpint(list->children->len, ==, 2);

  const Node *a = g_array_index(list->children, Node*, 0);
  const Node *b = g_array_index(list->children, Node*, 1);
  g_assert_cmpint(a->type, ==, LISP_AST_NODE_TYPE_SYMBOL);
  g_assert_cmpint(a->children->len, ==, 1);
  g_assert_cmpint(b->type, ==, LISP_AST_NODE_TYPE_SYMBOL);
  g_assert_cmpint(b->children->len, ==, 1);
  const Node *a_name = g_array_index(a->children, Node*, 0);
  const Node *b_name = g_array_index(b->children, Node*, 0);
  g_assert_cmpint(a_name->type, ==, LISP_AST_NODE_TYPE_SYMBOL_NAME);
  g_assert_cmpint(b_name->type, ==, LISP_AST_NODE_TYPE_SYMBOL_NAME);
  g_assert_cmpstr(a_name->start_token->text, ==, "a");
  g_assert_cmpstr(b_name->start_token->text, ==, "b");

  parser_fixture_free(&fixture);
}

static void test_missing_closing_paren(void) {
  ParserFixture fixture = parser_fixture_from_text("(a b");

  const Node *ast = lisp_parser_get_ast(fixture.parser);
  g_assert_cmpint(ast->children->len, ==, 1);

  const Node *list = g_array_index(ast->children, Node*, 0);
  g_assert_cmpint(list->type, ==, LISP_AST_NODE_TYPE_LIST);
  g_assert_cmpint(list->children->len, ==, 2);

  const Node *a = g_array_index(list->children, Node*, 0);
  const Node *b = g_array_index(list->children, Node*, 1);
  g_assert_cmpint(a->type, ==, LISP_AST_NODE_TYPE_SYMBOL);
  g_assert_cmpint(b->type, ==, LISP_AST_NODE_TYPE_SYMBOL);
  g_assert_cmpint(a->children->len, ==, 1);
  g_assert_cmpint(b->children->len, ==, 1);
  const Node *a_name = g_array_index(a->children, Node*, 0);
  const Node *b_name = g_array_index(b->children, Node*, 0);
  g_assert_cmpint(a_name->type, ==, LISP_AST_NODE_TYPE_SYMBOL_NAME);
  g_assert_cmpint(b_name->type, ==, LISP_AST_NODE_TYPE_SYMBOL_NAME);
  g_assert_cmpstr(a_name->start_token->text, ==, "a");
  g_assert_cmpstr(b_name->start_token->text, ==, "b");

  parser_fixture_free(&fixture);
}

static void test_symbol_with_package(void) {
  ParserFixture fixture = parser_fixture_from_text("pkg:foo");

  GArray *tokens = lisp_lexer_get_tokens(fixture.lexer);
  g_assert_cmpint(tokens->len, ==, 3);
  const LispToken *sep = &g_array_index(tokens, LispToken, 1);
  g_assert_cmpint(sep->type, ==, LISP_TOKEN_TYPE_SYMBOL_SEPARATOR);

  const Node *ast = lisp_parser_get_ast(fixture.parser);
  g_assert_cmpint(ast->children->len, ==, 1);
  const Node *sym = g_array_index(ast->children, Node*, 0);
  g_assert_cmpint(sym->type, ==, LISP_AST_NODE_TYPE_SYMBOL);
  g_assert_cmpint(sym->children->len, ==, 3);
  const Node *pkg = g_array_index(sym->children, Node*, 0);
  const Node *sep_node = g_array_index(sym->children, Node*, 1);
  const Node *name = g_array_index(sym->children, Node*, 2);
  g_assert_cmpint(pkg->type, ==, LISP_AST_NODE_TYPE_SYMBOL_PACKAGE);
  g_assert_cmpint(sep_node->type, ==, LISP_AST_NODE_TYPE_SYMBOL_SEPARATOR);
  g_assert_cmpint(name->type, ==, LISP_AST_NODE_TYPE_SYMBOL_NAME);
  g_assert_cmpstr(pkg->start_token->text, ==, "pkg");
  g_assert_cmpstr(sep_node->start_token->text, ==, ":");
  g_assert_cmpstr(name->start_token->text, ==, "foo");

  parser_fixture_free(&fixture);
}

static void test_extra_closing_paren(void) {
  ParserFixture fixture = parser_fixture_from_text(")");

  GArray *tokens = lisp_lexer_get_tokens(fixture.lexer);
  g_assert_cmpint(tokens->len, ==, 1);
  const LispToken *token = &g_array_index(tokens, LispToken, 0);
  g_assert_cmpint(token->type, ==, LISP_TOKEN_TYPE_LIST_END);

  const Node *ast = lisp_parser_get_ast(fixture.parser);
  g_assert_cmpint(ast->children->len, ==, 0);

  parser_fixture_free(&fixture);
}

static void test_comment(void) {
  ParserFixture fixture = parser_fixture_from_text("; a comment\n");

  GArray *tokens = lisp_lexer_get_tokens(fixture.lexer);
  g_assert_cmpint(tokens->len, ==, 2);
  const LispToken *token = &g_array_index(tokens, LispToken, 0);
  g_assert_cmpint(token->type, ==, LISP_TOKEN_TYPE_COMMENT);

  const Node *ast = lisp_parser_get_ast(fixture.parser);
  g_assert_cmpint(ast->children->len, ==, 0);

  parser_fixture_free(&fixture);
}

static void test_node_to_string(void) {
  const gchar *text = " (  + 1 ; comment\n   (- 2 3) ) ; trailing\n";
  ParserFixture fixture = parser_fixture_from_text(text);
  const Node *ast = lisp_parser_get_ast(fixture.parser);
  g_assert_cmpint(ast->children->len, ==, 1);
  const Node *list = g_array_index(ast->children, Node*, 0);
  gchar *s = node_to_string(list);
  g_assert_cmpstr(s, ==, "(+ 1 (- 2 3))");
  g_free(s);
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
  g_test_add_func("/lisp_parser/symbol_with_package", test_symbol_with_package);
  g_test_add_func("/lisp_parser/extra_closing_paren", test_extra_closing_paren);
  g_test_add_func("/lisp_parser/comment", test_comment);
  g_test_add_func("/lisp_parser/node_to_string", test_node_to_string);
  return g_test_run();
}

