#pragma once

#include <glib.h>
#include "lisp_lexer.h"
#include "marker_manager.h"

typedef struct _TokenManager TokenManager;

TokenManager *token_manager_new(MarkerManager *marker_manager);
void          token_manager_free(TokenManager *manager);
void          token_manager_clear(TokenManager *manager);
void          token_manager_set_tokens(TokenManager *manager, GArray *tokens);
const GArray *token_manager_get_tokens(TokenManager *manager);
GArray       *token_manager_peek_tokens(TokenManager *manager);
void          token_manager_replace_range(TokenManager *manager, guint start_index,
                                          guint end_index, GArray *tokens);
void          token_manager_update_tokens(TokenManager *manager, Document *document,
                                          gsize change_start, gsize change_end,
                                          gsize text_length, gsize *token_change_start,
                                          gsize *token_change_end);

