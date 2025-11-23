#include "token_manager.h"

struct _TokenManager {
  MarkerManager *marker_manager;
  GArray *tokens; /* owned, LispToken */
};

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
    if (token->start_marker)
      marker_manager_unref_marker(manager->marker_manager, token->start_marker);
    if (token->end_marker)
      marker_manager_unref_marker(manager->marker_manager, token->end_marker);
    g_free(token->text);
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

