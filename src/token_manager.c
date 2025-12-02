#include "token_manager.h"

struct _TokenManager {
  MarkerManager *marker_manager;
  GArray *tokens; /* owned, LispToken */
};

static void token_manager_free_token(TokenManager *manager, LispToken *token);
static gboolean token_manager_token_has_invalid_markers(const LispToken *token);
static gboolean token_manager_tokens_empty(TokenManager *manager, Document *document,
                                           gsize text_length);
static void token_manager_find_relex_indices(const GArray *tokens, gsize change_start,
                                             gsize change_end, guint *start_index,
                                             guint *end_index);
static void token_manager_compute_relex_bounds(const GArray *tokens, gsize text_length,
                                               guint start_index, guint end_index,
                                               gsize *relex_start, gsize *relex_end);

TokenManager *token_manager_new(MarkerManager *marker_manager) {
  g_return_val_if_fail(marker_manager != NULL, NULL);
  TokenManager *manager = g_new0(TokenManager, 1);
  manager->marker_manager = marker_manager;
  manager->tokens = NULL;
  return manager;
}

void token_manager_free(TokenManager *manager) {
  if (!manager)
    return;
  token_manager_clear(manager);
  g_free(manager);
}

void token_manager_clear(TokenManager *manager) {
  g_return_if_fail(manager != NULL);
  if (!manager->tokens)
    return;
  for (guint i = 0; i < manager->tokens->len; i++) {
    LispToken *token = &g_array_index(manager->tokens, LispToken, i);
    token_manager_free_token(manager, token);
  }
  g_array_free(manager->tokens, TRUE);
  manager->tokens = NULL;
}

void token_manager_set_tokens(TokenManager *manager, GArray *tokens) {
  g_return_if_fail(manager != NULL);
  token_manager_clear(manager);
  manager->tokens = tokens;
}

const GArray *token_manager_get_tokens(TokenManager *manager) {
  g_return_val_if_fail(manager != NULL, NULL);
  return manager->tokens;
}

GArray *token_manager_peek_tokens(TokenManager *manager) {
  g_return_val_if_fail(manager != NULL, NULL);
  return manager->tokens;
}

void token_manager_replace_range(TokenManager *manager, guint start_index, guint end_index,
                                 GArray *tokens) {
  g_return_if_fail(manager != NULL);

  if (!manager->tokens)
    manager->tokens = g_array_new(FALSE, TRUE, sizeof(LispToken));

  if (start_index > manager->tokens->len)
    start_index = manager->tokens->len;
  if (end_index > manager->tokens->len)
    end_index = manager->tokens->len;
  if (end_index < start_index)
    end_index = start_index;

  for (guint i = start_index; i < end_index; i++) {
    LispToken *token = &g_array_index(manager->tokens, LispToken, i);
    token_manager_free_token(manager, token);
  }

  if (end_index > start_index)
    g_array_remove_range(manager->tokens, start_index, end_index - start_index);

  if (tokens && tokens->len > 0)
    g_array_insert_vals(manager->tokens, start_index, tokens->data, tokens->len);

  if (tokens)
    g_array_free(tokens, TRUE);
}

void token_manager_update_tokens(TokenManager *manager, Document *document, gsize change_start,
                                 gsize change_end, gsize text_length,
                                 gsize *token_change_start, gsize *token_change_end) {
  g_return_if_fail(manager != NULL);
  g_return_if_fail(document != NULL);

  if (token_manager_tokens_empty(manager, document, text_length)) {
    if (token_change_start)
      *token_change_start = 0;
    if (token_change_end)
      *token_change_end = text_length;
    return;
  }

  GArray *tokens = token_manager_peek_tokens(manager);

  guint start_index = 0;
  guint end_index = 0;
  token_manager_find_relex_indices(tokens, change_start, change_end, &start_index, &end_index);

  end_index = tokens->len;

  gsize relex_start = 0;
  gsize relex_end = 0;
  token_manager_compute_relex_bounds(tokens, text_length, start_index, end_index, &relex_start,
                                     &relex_end);

  if (start_index == 0)
    relex_start = 0;

  if (relex_start > change_start) {
    if (start_index > 0) {
      const LispToken *previous = &g_array_index(tokens, LispToken, start_index - 1);
      relex_start = marker_get_offset(previous->start_marker);
    } else {
      relex_start = 0;
    }
  }

  if (relex_start > change_start)
    relex_start = change_start;

  GArray *replacement_tokens = lisp_lexer_lex_range(document, relex_start, relex_end);
  if (token_change_start)
    *token_change_start = relex_start;
  if (token_change_end)
    *token_change_end = relex_end;
  token_manager_replace_range(manager, start_index, end_index, replacement_tokens);
}

static void token_manager_free_token(TokenManager *manager, LispToken *token) {
  if (!token)
    return;
  if (token->start_marker)
    marker_manager_unref_marker(manager->marker_manager, token->start_marker);
  if (token->end_marker)
    marker_manager_unref_marker(manager->marker_manager, token->end_marker);
  g_free(token->text);
}

static gboolean token_manager_token_has_invalid_markers(const LispToken *token) {
  g_return_val_if_fail(token != NULL, TRUE);
  return !marker_is_valid(token->start_marker) || !marker_is_valid(token->end_marker);
}

static gboolean token_manager_tokens_empty(TokenManager *manager, Document *document,
                                           gsize text_length) {
  GArray *tokens = token_manager_peek_tokens(manager);
  if (tokens && tokens->len > 0)
    return FALSE;

  GArray *new_tokens = lisp_lexer_lex_range(document, 0, text_length);
  token_manager_set_tokens(manager, new_tokens);
  return TRUE;
}

static void token_manager_find_relex_indices(const GArray *tokens, gsize change_start,
                                             gsize change_end, guint *start_index,
                                             guint *end_index) {
  *start_index = tokens->len;
  *end_index = tokens->len;

  for (guint i = 0; i < tokens->len; i++) {
    const LispToken *token = &g_array_index(tokens, LispToken, i);
    gsize token_end = marker_get_offset(token->end_marker);
    // Changing the character at token_end+1 affects this token.
    if (token_manager_token_has_invalid_markers(token) || token_end + 1 >= change_start) {
      *start_index = i;
      break;
    }
  }

  for (guint i = *start_index; i < tokens->len; i++) {
    const LispToken *token = &g_array_index(tokens, LispToken, i);
    gsize token_start = marker_get_offset(token->start_marker);
    if (!token_manager_token_has_invalid_markers(token) && token_start <= change_end) {
      *end_index = i;
      break;
    }
  }
}

static void token_manager_compute_relex_bounds(const GArray *tokens, gsize text_length,
                                               guint start_index, guint end_index,
                                               gsize *relex_start, gsize *relex_end) {
  if (start_index < tokens->len) {
    const LispToken *token = &g_array_index(tokens, LispToken, start_index);
    *relex_start = marker_get_offset(token->start_marker);
  } else {
    *relex_start = text_length;
  }

  if (end_index < tokens->len) {
    const LispToken *token = &g_array_index(tokens, LispToken, end_index);
    *relex_end = marker_get_offset(token->start_marker);
  } else {
    *relex_end = text_length;
  }

  if (*relex_start > text_length)
    *relex_start = text_length;
  if (*relex_end > text_length)
    *relex_end = text_length;
  if (*relex_start > *relex_end)
    *relex_start = *relex_end;
}

