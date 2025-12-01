#include "document.h"
#include <glib.h>

static void assert_token(const GArray *tokens, guint index, LispTokenType type, const gchar *text) {
  const LispToken *token = &g_array_index((GArray *)tokens, LispToken, index);
  g_assert_cmpuint(token->type, ==, type);
  g_assert_cmpstr(token->text, ==, text);
}

static Document *document_with_content(const gchar *text) {
  Document *document = document_new(NULL, DOCUMENT_DORMANT);
  document_set_content(document, g_string_new(text));
  return document;
}

static void test_insert_relexes_changed_segment(void) {
  Document *document = document_with_content("foo bar baz");
  const GArray *original_tokens = document_get_tokens(document);
  g_assert_nonnull(original_tokens);
  g_assert_cmpuint(original_tokens->len, ==, 5);
  const LispToken *first_token = &g_array_index((GArray *)original_tokens, LispToken, 0);
  Marker *first_start = first_token->start_marker;
  Marker *first_end = first_token->end_marker;

  document_insert_text(document, 4, "new ", 4);

  const GArray *updated_tokens = document_get_tokens(document);
  g_assert_nonnull(updated_tokens);
  g_assert_cmpuint(updated_tokens->len, ==, 7);
  const LispToken *updated_first = &g_array_index((GArray *)updated_tokens, LispToken, 0);
  g_assert_true(updated_first->start_marker == first_start);
  g_assert_true(updated_first->end_marker == first_end);

  assert_token(updated_tokens, 0, LISP_TOKEN_TYPE_SYMBOL, "foo");
  assert_token(updated_tokens, 1, LISP_TOKEN_TYPE_WHITESPACE, " ");
  assert_token(updated_tokens, 2, LISP_TOKEN_TYPE_SYMBOL, "new");
  assert_token(updated_tokens, 3, LISP_TOKEN_TYPE_WHITESPACE, " ");
  assert_token(updated_tokens, 4, LISP_TOKEN_TYPE_SYMBOL, "bar");
  assert_token(updated_tokens, 5, LISP_TOKEN_TYPE_WHITESPACE, " ");
  assert_token(updated_tokens, 6, LISP_TOKEN_TYPE_SYMBOL, "baz");

  document_free(document);
}

static void test_delete_relexes_changed_segment(void) {
  Document *document = document_with_content("aaa bbb ccc");
  const GArray *original_tokens = document_get_tokens(document);
  g_assert_nonnull(original_tokens);
  g_assert_cmpuint(original_tokens->len, ==, 5);
  const LispToken *first_token = &g_array_index((GArray *)original_tokens, LispToken, 0);
  Marker *first_start = first_token->start_marker;
  Marker *first_end = first_token->end_marker;

  document_delete_text(document, 4, 8);

  const GArray *updated_tokens = document_get_tokens(document);
  g_assert_nonnull(updated_tokens);
  g_assert_cmpuint(updated_tokens->len, ==, 3);
  const LispToken *updated_first = &g_array_index((GArray *)updated_tokens, LispToken, 0);
  g_assert_true(updated_first->start_marker == first_start);
  g_assert_true(updated_first->end_marker == first_end);

  assert_token(updated_tokens, 0, LISP_TOKEN_TYPE_SYMBOL, "aaa");
  assert_token(updated_tokens, 1, LISP_TOKEN_TYPE_WHITESPACE, " ");
  assert_token(updated_tokens, 2, LISP_TOKEN_TYPE_SYMBOL, "ccc");

  document_free(document);
}

static void test_insert_splits_token_with_internal_space(void) {
  Document *document = document_with_content("foobaz");

  document_insert_text(document, 3, "qu ux", 5);

  const GArray *tokens = document_get_tokens(document);
  g_assert_nonnull(tokens);
  g_assert_cmpuint(tokens->len, ==, 3);

  assert_token(tokens, 0, LISP_TOKEN_TYPE_SYMBOL, "fooqu");
  assert_token(tokens, 1, LISP_TOKEN_TYPE_WHITESPACE, " ");
  assert_token(tokens, 2, LISP_TOKEN_TYPE_SYMBOL, "uxbaz");

  document_free(document);
}

static void test_insert_extends_previous_token(void) {
  Document *document = document_with_content("foo bar");

  const GArray *original_tokens = document_get_tokens(document);
  g_assert_nonnull(original_tokens);
  g_assert_cmpuint(original_tokens->len, ==, 3);
  assert_token(original_tokens, 0, LISP_TOKEN_TYPE_SYMBOL, "foo");
  assert_token(original_tokens, 1, LISP_TOKEN_TYPE_WHITESPACE, " ");
  assert_token(original_tokens, 2, LISP_TOKEN_TYPE_SYMBOL, "bar");

  const LispToken *original_first = &g_array_index((GArray *)original_tokens, LispToken, 0);
  g_assert_cmpuint(marker_get_offset(original_first->start_marker), ==, 0);
  g_assert_cmpuint(marker_get_offset(original_first->end_marker), ==, 3);

  document_insert_text(document, 3, "b", 1);

  g_assert_cmpstr(document_get_content(document)->str, ==, "foob bar");

  const GArray *tokens = document_get_tokens(document);
  g_assert_nonnull(tokens);
  g_assert_cmpuint(tokens->len, ==, 3);

  const LispToken *first_token = &g_array_index((GArray *)tokens, LispToken, 0);
  g_assert_cmpuint(marker_get_offset(first_token->start_marker), ==, 0);
  g_assert_cmpuint(marker_get_offset(first_token->end_marker), ==, 4);

  assert_token(tokens, 0, LISP_TOKEN_TYPE_SYMBOL, "foob");
  assert_token(tokens, 1, LISP_TOKEN_TYPE_WHITESPACE, " ");
  assert_token(tokens, 2, LISP_TOKEN_TYPE_SYMBOL, "bar");

  document_free(document);
}

static void test_delete_joins_adjacent_tokens(void) {
  Document *document = document_with_content("foo bar baz");

  document_delete_text(document, 3, 4);

  const GArray *tokens = document_get_tokens(document);
  g_assert_nonnull(tokens);
  g_assert_cmpuint(tokens->len, ==, 3);

  assert_token(tokens, 0, LISP_TOKEN_TYPE_SYMBOL, "foobar");
  assert_token(tokens, 1, LISP_TOKEN_TYPE_WHITESPACE, " ");
  assert_token(tokens, 2, LISP_TOKEN_TYPE_SYMBOL, "baz");

  document_free(document);
}

int main(int argc, char *argv[]) {
  g_test_init(&argc, &argv, NULL);
  g_test_add_func("/document/relex_insert", test_insert_relexes_changed_segment);
  g_test_add_func("/document/relex_delete", test_delete_relexes_changed_segment);
  g_test_add_func("/document/relex_insert_split_token", test_insert_splits_token_with_internal_space);
  g_test_add_func("/document/relex_insert_extend_token", test_insert_extends_previous_token);
  g_test_add_func("/document/relex_delete_join_tokens", test_delete_joins_adjacent_tokens);
  return g_test_run();
}
