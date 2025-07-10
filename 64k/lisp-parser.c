// Ensure all dependency headers are processed before our own header.
#include <glib.h>
#include <gtk/gtk.h>
#include "lisp-parser.h"
// lisp-parser.h also includes these, but explicit order might matter for macros.

// Define the LispParser structure
struct _LispParser {
    GNode *ast_root;
    GtkSourceBuffer *buffer; // Not owned by the parser
};

// --- LispSyntaxElement Helpers ---

/**
 * lisp_syntax_element_new: (static)
 * @type: The type of the syntax element.
 * @text: The text content of the element. Can be NULL. Will be duplicated.
 * @start: Pointer to the start GtkTextIter. Can be NULL.
 * @end: Pointer to the end GtkTextIter. Can be NULL.
 *
 * Allocates and initializes a new LispSyntaxElement.
 * Text is copied. Iterators are copied.
 *
 * Returns: (transfer full): A new LispSyntaxElement, or NULL on allocation failure.
 */
static LispSyntaxElement* lisp_syntax_element_new(LispSyntaxElementType type,
                                                  const gchar *text,
                                                  const GtkTextIter *start,
                                                  const GtkTextIter *end) {
    LispSyntaxElement *element = g_new(LispSyntaxElement, 1);
    if (!element) { // g_new aborts on failure by default, but defensive check is fine.
        return NULL;
    }

    element->type = type;
    element->text = text ? g_strdup(text) : NULL;

    if (start) {
        element->start_iter = *start;
    } else {
        memset(&element->start_iter, 0, sizeof(GtkTextIter)); // Initialize to a zeroed/invalid state
    }
    if (end) {
        element->end_iter = *end;
    } else {
        memset(&element->end_iter, 0, sizeof(GtkTextIter)); // Initialize to a zeroed/invalid state
    }

    return element;
}

/**
 * lisp_syntax_element_free: (Public, declared in header)
 * @data: A pointer to a LispSyntaxElement.
 *
 * Frees a LispSyntaxElement, including its text.
 */
void lisp_syntax_element_free(gpointer data) {
    if (!data) {
        return;
    }
    LispSyntaxElement *element = (LispSyntaxElement *)data;
    g_free(element->text);
    g_free(element);
}

// Adapter function for g_node_traverse to free LispSyntaxElement data
static gboolean lisp_syntax_element_free_node_data_cb(GNode *node, gpointer user_data G_GNUC_UNUSED) {
    if (node->data) {
        lisp_syntax_element_free(node->data);
        node->data = NULL;
    }
    return FALSE; // Continue traversal
}


// --- LispParser Implementation ---

LispParser *lisp_parser_new(GtkSourceBuffer *buffer) {
    g_return_val_if_fail(GTK_SOURCE_IS_BUFFER(buffer), NULL);

    LispParser *parser = g_new(LispParser, 1);

    parser->buffer = buffer;
    parser->ast_root = NULL;

    LispSyntaxElement *root_doc_element = lisp_syntax_element_new(LISP_SYNTAX_ELEMENT_DOCUMENT_ROOT,
                                                                  "DOCUMENT_ROOT", NULL, NULL);
    if (!root_doc_element) {
        g_free(parser);
        return NULL;
    }
    parser->ast_root = g_node_new(root_doc_element);
    if (!parser->ast_root) {
        lisp_syntax_element_free(root_doc_element);
        g_free(parser);
        return NULL;
    }

    return parser;
}

void lisp_parser_free(LispParser *parser) {
    g_return_if_fail(parser != NULL);

    if (parser->ast_root) {
        g_node_traverse(parser->ast_root,
                        G_POST_ORDER,
                        G_TRAVERSE_ALL,
                        -1,
                        lisp_syntax_element_free_node_data_cb,
                        NULL);

        g_node_destroy(parser->ast_root);
        parser->ast_root = NULL;
    }

    g_free(parser);
}

GNode *lisp_parser_get_ast_root(LispParser *parser) {
    g_return_val_if_fail(parser != NULL, NULL);
    return parser->ast_root;
}

void lisp_parser_parse(LispParser *parser) {
    g_return_if_fail(parser != NULL);
    g_return_if_fail(GTK_SOURCE_IS_BUFFER(parser->buffer));

    // 1. Clear existing AST data and structure
    if (parser->ast_root) {
        g_node_traverse(parser->ast_root, G_POST_ORDER, G_TRAVERSE_ALL, -1,
                        lisp_syntax_element_free_node_data_cb, NULL);
        g_node_destroy(parser->ast_root);
        parser->ast_root = NULL;
    }

    // 2. Create a new root for the new parse
    LispSyntaxElement *root_element = lisp_syntax_element_new(LISP_SYNTAX_ELEMENT_DOCUMENT_ROOT,
                                                              "DOCUMENT_ROOT", NULL, NULL);
    if (!root_element) {
        g_warning("Failed to create root syntax element for parsing.");
        return;
    }
    parser->ast_root = g_node_new(root_element);
    if (!parser->ast_root) {
        lisp_syntax_element_free(root_element);
        g_warning("Failed to create root GNode for parsing AST.");
        return;
    }

    // --- Actual parsing logic ---

    GtkTextIter current_iter, token_start_iter;
    gtk_text_buffer_get_start_iter(GTK_TEXT_BUFFER(parser->buffer), &current_iter);

    // Stack to keep track of current parent GNode for nesting lists
    GArray *node_stack = g_array_new(FALSE, FALSE, sizeof(GNode*));
    g_array_append_val(node_stack, parser->ast_root); // Start with document root as current parent

    while (!gtk_text_iter_is_end(&current_iter)) {
        gunichar current_char = gtk_text_iter_get_char(&current_iter);
        LispSyntaxElementType current_element_type;
        gchar *token_text = NULL;

        token_start_iter = current_iter; // Mark start of the current token

        if (current_char == '(') {
            current_element_type = LISP_SYNTAX_ELEMENT_LIST_START;
            GtkTextIter temp_end_iter = current_iter;
            gtk_text_iter_forward_char(&temp_end_iter);
            token_text = gtk_text_iter_get_text(&token_start_iter, &temp_end_iter);
            current_iter = temp_end_iter; // Advance main iterator
        } else if (current_char == ')') {
            current_element_type = LISP_SYNTAX_ELEMENT_LIST_END;
            GtkTextIter temp_end_iter = current_iter;
            gtk_text_iter_forward_char(&temp_end_iter);
            token_text = gtk_text_iter_get_text(&token_start_iter, &temp_end_iter);
            current_iter = temp_end_iter; // Advance main iterator
        } else if (current_char == '"') { // String
            GtkTextIter str_end_iter = current_iter;
            gtk_text_iter_forward_char(&str_end_iter); // Move past opening "
            gboolean found_end_quote = FALSE;
            while (!gtk_text_iter_is_end(&str_end_iter)) {
                gunichar str_char = gtk_text_iter_get_char(&str_end_iter);
                if (str_char == '\\') {
                    gtk_text_iter_forward_char(&str_end_iter); // Skip char after '\'
                    if (gtk_text_iter_is_end(&str_end_iter)) break;
                } else if (str_char == '"') {
                    found_end_quote = TRUE;
                    gtk_text_iter_forward_char(&str_end_iter); // Include closing "
                    break;
                }
                gtk_text_iter_forward_char(&str_end_iter);
            }
            token_text = gtk_text_iter_get_text(&token_start_iter, &str_end_iter);
            current_element_type = found_end_quote ? LISP_SYNTAX_ELEMENT_STRING : LISP_SYNTAX_ELEMENT_INCOMPLETE_STRING;
            current_iter = str_end_iter; // Advance main iterator
        } else if (current_char == ';') { // Comment
            current_element_type = LISP_SYNTAX_ELEMENT_COMMENT;
            GtkTextIter comment_end_iter = current_iter;
            while (!gtk_text_iter_is_end(&comment_end_iter) && gtk_text_iter_get_char(&comment_end_iter) != '\n') {
                gtk_text_iter_forward_char(&comment_end_iter);
            }
            // Optionally include newline: if (!gtk_text_iter_is_end(&comment_end_iter)) gtk_text_iter_forward_char(&comment_end_iter);
            token_text = gtk_text_iter_get_text(&token_start_iter, &comment_end_iter);
            current_iter = comment_end_iter; // Advance main iterator
        } else if (g_unichar_isspace(current_char)) {
            current_element_type = LISP_SYNTAX_ELEMENT_WHITESPACE;
            GtkTextIter ws_end_iter = current_iter;
            while (!gtk_text_iter_is_end(&ws_end_iter) && g_unichar_isspace(gtk_text_iter_get_char(&ws_end_iter))) {
                gtk_text_iter_forward_char(&ws_end_iter);
            }
            token_text = gtk_text_iter_get_text(&token_start_iter, &ws_end_iter);
            current_iter = ws_end_iter; // Advance main iterator
        } else { // Atom
            current_element_type = LISP_SYNTAX_ELEMENT_ATOM;
            GtkTextIter atom_end_iter = current_iter;
            while (!gtk_text_iter_is_end(&atom_end_iter)) {
                gunichar atom_char = gtk_text_iter_get_char(&atom_end_iter);
                if (g_unichar_isspace(atom_char) || atom_char == '(' || atom_char == ')' ||
                    atom_char == '"' || atom_char == ';') {
                    break;
                }
                gtk_text_iter_forward_char(&atom_end_iter);
            }
            token_text = gtk_text_iter_get_text(&token_start_iter, &atom_end_iter);
            current_iter = atom_end_iter; // Advance main iterator

            if (token_text == NULL || strlen(token_text) == 0) {
                g_free(token_text); // token_text would be "" if start == end, g_strdup("") is valid
                // This can happen if buffer ends with a delimiter or multiple delimiters
                // or if gtk_text_iter_is_end(&current_iter) was false but current_iter was already at the very end.
                // No element to create, just continue.
                continue;
            }
        }

        LispSyntaxElement *element = lisp_syntax_element_new(current_element_type, token_text, &token_start_iter, &current_iter);
        g_free(token_text);

        if (!element) {
            g_warning("Failed to create syntax element for token. Continuing.");
            continue;
        }

        GNode *current_parent_node = g_array_index(node_stack, GNode*, node_stack->len - 1);
        GNode *new_gnode = g_node_new(element);
        if(!new_gnode){
            lisp_syntax_element_free(element);
            g_warning("Failed to create GNode for syntax element. Continuing.");
            continue;
        }
        g_node_append(current_parent_node, new_gnode);

        if (element->type == LISP_SYNTAX_ELEMENT_LIST_START) {
            g_array_append_val(node_stack, new_gnode); // Push the new list node as current parent
        } else if (element->type == LISP_SYNTAX_ELEMENT_LIST_END) {
            if (node_stack->len > 1) { // If stack has more than just the document root
                GNode *top_node_on_stack = g_array_index(node_stack, GNode*, node_stack->len - 1);
                LispSyntaxElement *top_node_data = (LispSyntaxElement*) top_node_on_stack->data;

                // The LIST_END's direct parent should be the LIST_START it's closing.
                // If the top of the stack is that parent (the LIST_START node), then pop it.
                if (top_node_on_stack == current_parent_node && top_node_data->type == LISP_SYNTAX_ELEMENT_LIST_START) {
                     g_array_remove_index_fast(node_stack, node_stack->len - 1);
                } else {
                    // This ')' does not match the list node on top of stack (current_parent_node).
                    // Or, current_parent_node is not a LIST_START (e.g. root)
                    // This means it's an unmatched ')' for the current direct parent.
                    // The element type of this ')' should be UNMATCHED_CLOSE_PAREN.
                    // This check is a bit tricky. The `current_parent_node` IS the LIST_START node.
                    // So if element type is LIST_END, we pop `current_parent_node` from stack.
                    GNode* just_popped_list_start_node = g_array_index(node_stack, GNode*, node_stack->len-1);
                    LispSyntaxElement* data_of_list_being_closed = (LispSyntaxElement*)just_popped_list_start_node->data;

                    if (data_of_list_being_closed->type == LISP_SYNTAX_ELEMENT_LIST_START || data_of_list_being_closed->type == LISP_SYNTAX_ELEMENT_INCOMPLETE_LIST) {
                         g_array_remove_index_fast(node_stack, node_stack->len-1);
                    } else {
                        // This indicates an unmatched closing parenthesis.
                        element->type = LISP_SYNTAX_ELEMENT_UNMATCHED_CLOSE_PAREN;
                    }

                }
            } else { // Stack only contains document root, so this ')' is definitely unmatched
                element->type = LISP_SYNTAX_ELEMENT_UNMATCHED_CLOSE_PAREN;
            }
        }
    }

    // After processing the whole buffer, check the stack for unclosed lists
    while (node_stack->len > 1) { // Any nodes other than the document root still on stack?
        GNode *unclosed_node = g_array_index(node_stack, GNode*, node_stack->len - 1);
        LispSyntaxElement *element_data = (LispSyntaxElement *)unclosed_node->data;
        if (element_data && element_data->type == LISP_SYNTAX_ELEMENT_LIST_START) {
            element_data->type = LISP_SYNTAX_ELEMENT_INCOMPLETE_LIST;
        }
        // Also handle other potentially stack-managed types if any were introduced
        g_array_remove_index_fast(node_stack, node_stack->len - 1);
    }

    g_array_free(node_stack, TRUE);
    // g_message("LispParser: Parsing complete. AST Root: %p", (void*)parser->ast_root);
}
