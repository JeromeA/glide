#pragma once

#include <glib.h>
#include <gtk/gtk.h>
#include <gtksourceview/gtksource.h> // For GtkSourceBuffer

G_BEGIN_DECLS

// Forward declaration for LispParser
typedef struct _LispParser LispParser;

// Enum for different types of syntax elements
typedef enum {
    LISP_SYNTAX_ELEMENT_DOCUMENT_ROOT,  // Root of the AST
    LISP_SYNTAX_ELEMENT_ATOM,           // Symbols, numbers, etc.
    LISP_SYNTAX_ELEMENT_LIST_START,     // '('
    LISP_SYNTAX_ELEMENT_LIST_END,       // ')'
    LISP_SYNTAX_ELEMENT_STRING,         // "a string"
    LISP_SYNTAX_ELEMENT_COMMENT,        // ; a comment
    LISP_SYNTAX_ELEMENT_WHITESPACE,
    LISP_SYNTAX_ELEMENT_INVALID_CHAR,   // A character that doesn't belong
    LISP_SYNTAX_ELEMENT_UNMATCHED_CLOSE_PAREN, // A ')' without a matching '('
    LISP_SYNTAX_ELEMENT_INCOMPLETE_LIST, // A list that was started but not closed by EOF
    LISP_SYNTAX_ELEMENT_INCOMPLETE_STRING // A string that was started but not closed by EOF
} LispSyntaxElementType;

// Structure to hold information about each syntax element
typedef struct {
    LispSyntaxElementType type;
    gchar *text; // The actual text of the token, owned by this struct
    GtkTextIter start_iter; // Start position in the GtkTextBuffer
    GtkTextIter end_iter;   // End position in the GtkTextBuffer
} LispSyntaxElement;

// Public API for the LispParser

/**
 * lisp_parser_new:
 * @buffer: The GtkSourceBuffer to parse.
 *
 * Creates a new LispParser instance for the given buffer.
 * The parser does not take ownership of the buffer.
 *
 * Returns: (transfer full): A new LispParser instance.
 */
LispParser *lisp_parser_new(GtkSourceBuffer *buffer);

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
 * Parses or re-parses the content of the GtkSourceBuffer associated with the parser.
 * The existing AST in the parser is destroyed and a new one is built.
 */
void lisp_parser_parse(LispParser *parser);

/**
 * lisp_parser_get_ast_root:
 * @parser: The LispParser instance.
 *
 * Gets the root GNode of the Abstract Syntax Tree (AST).
 * The GNode and its children contain LispSyntaxElement data.
 * The returned GNode is owned by the parser and should not be freed by the caller.
 * Its lifetime is tied to the parser or until the next call to `lisp_parser_parse`.
 *
 * Returns: (transfer none): The root GNode of the AST.
 */
GNode *lisp_parser_get_ast_root(LispParser *parser);

/**
 * lisp_syntax_element_free:
 * @data: (transfer full): A pointer to a LispSyntaxElement to be freed.
 *
 * Frees the memory allocated for a LispSyntaxElement, including its text.
 * This function is suitable for use with GNode data freeing (e.g., g_node_destroy).
 */
void lisp_syntax_element_free(gpointer data);

G_END_DECLS
