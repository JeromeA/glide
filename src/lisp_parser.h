#pragma once

#include <glib.h>
#include "text_provider.h"

G_BEGIN_DECLS

// Forward declaration for LispParser
typedef struct _LispParser LispParser;

// Enum for different types of tokens
typedef enum {
    LISP_TOKEN_TYPE_NUMBER,
    LISP_TOKEN_TYPE_SYMBOL,
    LISP_TOKEN_TYPE_LIST_START,     // (
    LISP_TOKEN_TYPE_LIST_END,       // )
    LISP_TOKEN_TYPE_STRING,
    LISP_TOKEN_TYPE_COMMENT,
    LISP_TOKEN_TYPE_WHITESPACE,
    LISP_TOKEN_TYPE_INCOMPLETE_STRING,
    // Note: No 'unmatched close paren' token type; that's a parsing error, not a token type.
} LispTokenType;

// Structure for a single token
typedef struct {
    LispTokenType type;
    gchar *text;
    gsize start_offset;
    gsize end_offset;
} LispToken;

// Enum for AST node types
typedef enum {
    LISP_AST_NODE_TYPE_NUMBER,
    LISP_AST_NODE_TYPE_SYMBOL,
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
    GArray *children;             // Array of LispAstNode* pointers, for lists only.
};

// Public API for the LispParser

/**
 * lisp_parser_new:
 * @provider: The TextProvider to parse.
 *
 * Creates a new LispParser instance for the given provider.
 * The parser does not take ownership of the provider.
 *
 * Returns: (transfer full): A new LispParser instance.
 */
LispParser *lisp_parser_new(TextProvider *provider);

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
 *
 * Parses or re-parses the content provided by the TextProvider associated with the parser.
 * The existing AST in the parser is destroyed and a new one is built.
 */
void lisp_parser_parse(LispParser *parser);

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

/**
 * lisp_parser_get_tokens:
 * @parser: The LispParser instance.
 * @n_tokens: (out): Pointer to store the number of tokens.
 *
 * Gets the stream of tokens from the last parse.
 * The returned array is owned by the parser and should not be freed.
 *
 * Returns: (transfer none) (array length=n_tokens): The array of LispToken.
 */
const LispToken *lisp_parser_get_tokens(LispParser *parser, guint *n_tokens);

G_END_DECLS
