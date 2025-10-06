#pragma once

#include <glib.h>
#include "lisp_lexer.h"
#include "lisp_parser.h"

typedef struct _Project Project;

typedef enum {
  DOCUMENT_DORMANT,
  DOCUMENT_LIVE
} DocumentState;

typedef struct _Document Document;

typedef enum {
  DOCUMENT_ERROR_TYPE_GENERIC,
  DOCUMENT_ERROR_TYPE_UNRESOLVED_SYMBOL,
  DOCUMENT_ERROR_TYPE_UNDEFINED_FUNCTION
} DocumentErrorType;

typedef struct {
  gsize start;
  gsize end;
  DocumentErrorType type;
  gchar *message;
} DocumentError;

Document    *document_new(Project *project, GString *content,
    const gchar *path, DocumentState state);
Document    *document_new_virtual(GString *content);
void         document_free(Document *document);
DocumentState document_get_state(Document *document);
void         document_set_state(Document *document, DocumentState state);
void         document_set_content(Document *document, GString *content);
const GString *document_get_content(Document *document);
const GArray  *document_get_tokens(Document *document);
const Node    *document_get_ast(Document *document);
void         document_set_tokens(Document *document, GArray *tokens);
void         document_set_ast(Document *document, Node *ast);
const gchar *document_get_path(Document *document); /* borrowed */
void         document_set_path(Document *document, const gchar *path);
Document    *document_load(Project *project, const gchar *path);
const gchar *document_get_relative_path(Document *document);
void         document_clear_errors(Document *document);
void         document_clear_errors_of_type(Document *document,
    DocumentErrorType type);
void         document_add_error(Document *document, DocumentError error);
const GArray *document_get_errors(Document *document);
