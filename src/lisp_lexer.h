#pragma once

#include <glib.h>

G_BEGIN_DECLS

typedef struct _LispLexer LispLexer;

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
  gsize start_offset;
  gsize end_offset;
} LispToken;

LispLexer *lisp_lexer_new(void);
void       lisp_lexer_free(LispLexer *lexer);
GArray    *lisp_lexer_lex(LispLexer *lexer, const GString *text);

G_END_DECLS
