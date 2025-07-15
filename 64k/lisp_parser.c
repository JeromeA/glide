// Ensure all dependency headers are processed before our own header.
#include <glib.h>
#include "lisp_parser.h"

// Define the LispParser structure
struct _LispParser {
    TextProvider *provider; // Not owned
    GArray *tokens;          // Owns LispToken structs
    LispAstNode *ast;        // Owns the AST
};

// --- Forward Declarations for Static Functions ---
static void lisp_parser_clear_data(LispParser *parser);
static void lisp_token_free(gpointer token);
static void lisp_ast_node_free(LispAstNode *node);
static LispAstNode* parse_expression(const GArray *tokens, guint *position);

// --- Memory Management ---

static void lisp_token_free(gpointer token) {
    if (!token) return;
    LispToken *t = (LispToken*)token;
    g_free(t->text);
    // The struct itself is part of the GArray's chunk, so no need to g_free(t)
}

static void lisp_ast_node_free(LispAstNode *node) {
    if (!node) return;

    if (node->children) {
        for (guint i = 0; i < node->children->len; i++) {
            lisp_ast_node_free(g_array_index(node->children, LispAstNode*, i));
        }
        g_array_free(node->children, TRUE);
    }
    g_free(node);
}

// Clears tokens and AST from the parser
static void lisp_parser_clear_data(LispParser *parser) {
    if (parser->tokens) {
        g_array_free(parser->tokens, TRUE);
        parser->tokens = NULL;
    }
    if (parser->ast) {
        lisp_ast_node_free(parser->ast);
        parser->ast = NULL;
    }
}


// --- LispParser Implementation ---

LispParser *lisp_parser_new(TextProvider *provider) {
    g_return_val_if_fail(GLIDE_IS_TEXT_PROVIDER(provider), NULL);

    LispParser *parser = g_new0(LispParser, 1);
    parser->provider = provider;

    return parser;
}

void lisp_parser_free(LispParser *parser) {
    g_return_if_fail(parser != NULL);
    lisp_parser_clear_data(parser);
    g_free(parser);
}

const LispAstNode *lisp_parser_get_ast(LispParser *parser) {
    g_return_val_if_fail(parser != NULL, NULL);
    return parser->ast;
}

const LispToken *lisp_parser_get_tokens(LispParser *parser, guint *n_tokens) {
    g_return_val_if_fail(parser != NULL, NULL);
    if (n_tokens) {
        *n_tokens = parser->tokens ? parser->tokens->len : 0;
    }
    return parser->tokens ? (const LispToken*)parser->tokens->data : NULL;
}

void lisp_parser_parse(LispParser *parser) {
    g_return_if_fail(parser != NULL);
    g_return_if_fail(GLIDE_IS_TEXT_PROVIDER(parser->provider));

    // 1. Clear previous results
    lisp_parser_clear_data(parser);

    // Initialize storage for new results
    parser->tokens = g_array_new(FALSE, TRUE, sizeof(LispToken));
    g_array_set_clear_func(parser->tokens, lisp_token_free);

    // 2. Tokenization Stage
    gsize len = text_provider_get_length(parser->provider);
    gsize offset = 0;

    while (offset < len) {
        gunichar current_char = text_provider_get_char(parser->provider, offset);
        LispToken token = {0};
        token.start_offset = offset;

        if (g_unichar_isspace(current_char)) {
            token.type = LISP_TOKEN_TYPE_WHITESPACE;
            gsize end = offset;
            while (end < len && g_unichar_isspace(text_provider_get_char(parser->provider, end))) {
                end = text_provider_next_offset(parser->provider, end);
            }
            token.end_offset = end;
            offset = end;
        } else if (current_char == ';') {
            token.type = LISP_TOKEN_TYPE_COMMENT;
            gsize end = offset;
            while (end < len && text_provider_get_char(parser->provider, end) != '\n') {
                end = text_provider_next_offset(parser->provider, end);
            }
            token.end_offset = end;
            offset = end;
        } else if (current_char == '(') {
            token.type = LISP_TOKEN_TYPE_LIST_START;
            offset = text_provider_next_offset(parser->provider, offset);
            token.end_offset = offset;
        } else if (current_char == ')') {
            token.type = LISP_TOKEN_TYPE_LIST_END;
            offset = text_provider_next_offset(parser->provider, offset);
            token.end_offset = offset;
        } else if (current_char == '"') {
            gsize end = text_provider_next_offset(parser->provider, offset); /* skip opening quote */
            gboolean escaped = FALSE;
            gboolean found_end = FALSE;
            while (end < len) {
                gunichar c = text_provider_get_char(parser->provider, end);
                if (escaped) {
                    escaped = FALSE;
                } else if (c == '\\') {
                    escaped = TRUE;
                } else if (c == '"') {
                    found_end = TRUE;
                    end = text_provider_next_offset(parser->provider, end);
                    break;
                }
                end = text_provider_next_offset(parser->provider, end);
            }
            token.type = found_end ? LISP_TOKEN_TYPE_STRING : LISP_TOKEN_TYPE_INCOMPLETE_STRING;
            token.end_offset = end;
            offset = end;
        } else { /* Atom */
            token.type = LISP_TOKEN_TYPE_ATOM;
            gsize end = offset;
            while (end < len) {
                gunichar c = text_provider_get_char(parser->provider, end);
                if (g_unichar_isspace(c) || c == '(' || c == ')' || c == '"' || c == ';') {
                    break;
                }
                end = text_provider_next_offset(parser->provider, end);
            }
            token.end_offset = end;
            offset = end;
        }

        token.text = text_provider_get_text(parser->provider, token.start_offset, token.end_offset);
        g_array_append_val(parser->tokens, token);
    }

    // 3. AST Construction Stage
    guint position = 0;
    // The AST for a file is implicitly a list of top-level expressions.
    // We'll create a root node to hold them.
    parser->ast = g_new0(LispAstNode, 1);
    parser->ast->type = LISP_AST_NODE_TYPE_LIST; // Root is a list of expressions
    parser->ast->children = g_array_new(FALSE, FALSE, sizeof(LispAstNode*));
    // Root node doesn't correspond to a specific token.

    while(position < parser->tokens->len) {
        // Skip non-content tokens at the top level
        const LispToken *token = &g_array_index(parser->tokens, LispToken, position);
        if(token->type == LISP_TOKEN_TYPE_WHITESPACE || token->type == LISP_TOKEN_TYPE_COMMENT) {
            position++;
            continue;
        }

        LispAstNode *expr = parse_expression(parser->tokens, &position);
        if (expr) {
            g_array_append_val(parser->ast->children, expr);
        } else {
            // Invalid expression; skip and continue parsing the rest
            continue;
        }
    }
}

static LispAstNode* parse_expression(const GArray *tokens, guint *position) {
    // Skip leading whitespace/comments for the current expression
    while (*position < tokens->len) {
        const LispToken *token = &g_array_index(tokens, LispToken, *position);
        if (token->type != LISP_TOKEN_TYPE_WHITESPACE && token->type != LISP_TOKEN_TYPE_COMMENT) {
            break;
        }
        (*position)++;
    }

    if (*position >= tokens->len) {
        return NULL; // End of tokens
    }

    const LispToken *token = &g_array_index(tokens, LispToken, *position);

    if (token->type == LISP_TOKEN_TYPE_LIST_START) {
        // --- Parse a list ---
        LispAstNode *list_node = g_new0(LispAstNode, 1);
        list_node->type = LISP_AST_NODE_TYPE_LIST;
        list_node->start_token = token;
        list_node->children = g_array_new(FALSE, FALSE, sizeof(LispAstNode*));

        (*position)++; // Consume '('

        while (*position < tokens->len) {
            const LispToken *current_token = &g_array_index(tokens, LispToken, *position);

            if (current_token->type == LISP_TOKEN_TYPE_LIST_END) {
                list_node->end_token = current_token;
                (*position)++; // Consume ')'
                return list_node;
            }

            // Skip whitespace and comments inside the list
            if (current_token->type == LISP_TOKEN_TYPE_WHITESPACE || current_token->type == LISP_TOKEN_TYPE_COMMENT) {
                (*position)++;
                continue;
            }

            LispAstNode *child_expr = parse_expression(tokens, position);
            if (child_expr) {
                g_array_append_val(list_node->children, child_expr);
            } else {
                // Ignore invalid tokens inside lists
                continue;
            }
        }

        // If we exit the loop here, the list was not closed. We still create
        // the list node and pretend a closing parenthesis existed at EOF.
        list_node->end_token = NULL;
        return list_node;

    } else if (token->type == LISP_TOKEN_TYPE_ATOM || token->type == LISP_TOKEN_TYPE_STRING) {
        // --- Parse an atom or string ---
        LispAstNode *atom_node = g_new0(LispAstNode, 1);
        atom_node->type = (token->type == LISP_TOKEN_TYPE_STRING) ? LISP_AST_NODE_TYPE_STRING : LISP_AST_NODE_TYPE_ATOM;
        atom_node->start_token = token;
        atom_node->end_token = token; // For atoms/strings, start and end are the same.
        atom_node->children = NULL;

        (*position)++; // Consume the token
        return atom_node;

    } else if (token->type == LISP_TOKEN_TYPE_LIST_END) {
        // --- Unmatched closing parenthesis ---
        // Consume and ignore it
        (*position)++;
        return NULL;
    }

    // Should not be reached if token types are comprehensive
    (*position)++;
    return NULL;
}
