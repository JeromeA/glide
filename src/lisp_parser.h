#pragma once

#include <glib.h>
#include "lisp_lexer.h"

G_BEGIN_DECLS

typedef struct _LispParser LispParser;

// Enum for AST node types
typedef enum {
    LISP_AST_NODE_TYPE_NUMBER,
    LISP_AST_NODE_TYPE_SYMBOL,
    LISP_AST_NODE_TYPE_SYMBOL_PACKAGE,
    LISP_AST_NODE_TYPE_SYMBOL_SEPARATOR,
    LISP_AST_NODE_TYPE_SYMBOL_NAME,
    LISP_AST_NODE_TYPE_LIST,
    LISP_AST_NODE_TYPE_STRING,
    // Comments and whitespace are not typically included in the AST
} LispAstNodeType;

// Forward declaration
typedef struct _LispAstNode LispAstNode;

// Structure for an AST node
struct _LispAstNode {
    LispAstNodeType type;
    const LispToken *start_token; // For a list, the '('. For an atom, the token itself.
    const LispToken *end_token;   // For a list, the ')'. For an atom, same as start_token.
    GArray *children;             // Array of LispAstNode* pointers, for lists and symbols.
};

// Public API for the LispParser

/**
 * lisp_parser_new:
 * Creates a new LispParser instance.
 *
 * Returns: (transfer full): A new LispParser instance.
 */
LispParser *lisp_parser_new(void);

/**
 * lisp_parser_free:
 * @parser: (transfer full): The LispParser to free.
 *
 * Frees the LispParser and all its associated resources, including the AST.
 */
void lisp_parser_free(LispParser *parser);

/**
 * lisp_parser_parse:
 * @parser: The LispParser instance.
 * @tokens: (nullable): The tokens to parse.
 *
 * Parses the provided tokens and builds a new AST. Any existing AST is destroyed.
 */
void lisp_parser_parse(LispParser *parser, GArray *tokens);

/**
 * lisp_parser_get_ast:
 * @parser: The LispParser instance.
 *
 * Gets the root node of the Abstract Syntax Tree (AST).
 * The returned AST is owned by the parser and should not be freed.
 * Its lifetime is tied to the parser or until the next `lisp_parser_parse`.
 *
 * Returns: (transfer none): The root LispAstNode of the AST, or NULL if parsing failed.
 */
const LispAstNode *lisp_parser_get_ast(LispParser *parser);

G_END_DECLS
