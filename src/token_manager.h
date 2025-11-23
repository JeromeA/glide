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

