#pragma once

#include <glib.h>
#include "marker_manager.h"

G_BEGIN_DECLS

typedef struct _Document Document;

typedef enum {
  LISP_TOKEN_TYPE_NUMBER,
  LISP_TOKEN_TYPE_SYMBOL,
  LISP_TOKEN_TYPE_SYMBOL_SEPARATOR,
  LISP_TOKEN_TYPE_LIST_START,
  LISP_TOKEN_TYPE_LIST_END,
  LISP_TOKEN_TYPE_QUOTE,
  LISP_TOKEN_TYPE_BACKQUOTE,
  LISP_TOKEN_TYPE_UNQUOTE,
  LISP_TOKEN_TYPE_UNQUOTE_SPLICING,
  LISP_TOKEN_TYPE_STRING,
  LISP_TOKEN_TYPE_COMMENT,
  LISP_TOKEN_TYPE_WHITESPACE,
  LISP_TOKEN_TYPE_INCOMPLETE_STRING,
} LispTokenType;

typedef struct {
  LispTokenType type;
  gchar *text;
  Marker *start_marker;
  Marker *end_marker;
} LispToken;

GArray    *lisp_lexer_lex(Document *document);

G_END_DECLS
