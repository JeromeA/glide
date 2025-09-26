// Ensure all dependency headers are processed before our own header.
#include <glib.h>
#include "util.h"
#include "lisp_lexer.h"

static void lisp_token_free(gpointer token);
static inline gunichar gstring_get_char(const GString *text, gsize offset);
static inline gsize gstring_next_offset(const GString *text, gsize offset);
static inline gchar *gstring_slice_dup(const GString *text, gsize start, gsize end);

static void lisp_token_free(gpointer token) {
  if (!token) return;
  LispToken *t = token;
  g_free(t->text);
}

static inline gunichar gstring_get_char(const GString *text, gsize offset) {
  if (!text || offset >= text->len)
    return 0;
  return g_utf8_get_char(text->str + offset);
}

static inline gsize gstring_next_offset(const GString *text, gsize offset) {
  if (!text || offset >= text->len)
    return text ? text->len : 0;
  const gchar *ptr = text->str + offset;
  ptr = utf8_next_char(ptr);
  return (gsize)(ptr - text->str);
}

static inline gchar *gstring_slice_dup(const GString *text, gsize start, gsize end) {
  if (!text || start >= end)
    return g_strdup("");
  if (end > text->len)
    end = text->len;
  if (start > end)
    start = end;
  return g_strndup(text->str + start, end - start);
}

GArray *lisp_lexer_lex(const GString *text) {
  g_return_val_if_fail(text != NULL, NULL);

  GArray *tokens = g_array_new(FALSE, TRUE, sizeof(LispToken));
  g_array_set_clear_func(tokens, lisp_token_free);

  gsize len = text->len;
  gsize offset = 0;

  while (offset < len) {
    gunichar current_char = gstring_get_char(text, offset);
    LispToken token = {0};
    token.start_offset = offset;

    if (g_unichar_isspace(current_char)) {
      token.type = LISP_TOKEN_TYPE_WHITESPACE;
      gsize end = offset;
      while (end < len && g_unichar_isspace(gstring_get_char(text, end)))
        end = gstring_next_offset(text, end);
      token.end_offset = end;
      offset = end;
    } else if (current_char == ';') {
      token.type = LISP_TOKEN_TYPE_COMMENT;
      gsize end = offset;
      while (end < len && gstring_get_char(text, end) != '\n')
        end = gstring_next_offset(text, end);
      token.end_offset = end;
      offset = end;
    } else if (current_char == '(') {
      token.type = LISP_TOKEN_TYPE_LIST_START;
      offset = gstring_next_offset(text, offset);
      token.end_offset = offset;
    } else if (current_char == ')') {
      token.type = LISP_TOKEN_TYPE_LIST_END;
      offset = gstring_next_offset(text, offset);
      token.end_offset = offset;
    } else if (current_char == 0x27) {
      token.type = LISP_TOKEN_TYPE_QUOTE;
      offset = gstring_next_offset(text, offset);
      token.end_offset = offset;
    } else if (current_char == '`') {
      token.type = LISP_TOKEN_TYPE_BACKQUOTE;
      offset = gstring_next_offset(text, offset);
      token.end_offset = offset;
    } else if (current_char == ',') {
      gsize end = gstring_next_offset(text, offset);
      if (end < len && gstring_get_char(text, end) == '@') {
        end = gstring_next_offset(text, end);
        token.type = LISP_TOKEN_TYPE_UNQUOTE_SPLICING;
      } else {
        token.type = LISP_TOKEN_TYPE_UNQUOTE;
      }
      token.end_offset = end;
      offset = end;
    } else if (current_char == ':') {
      token.type = LISP_TOKEN_TYPE_SYMBOL_SEPARATOR;
      gsize end = gstring_next_offset(text, offset);
      if (end < len && gstring_get_char(text, end) == ':')
        end = gstring_next_offset(text, end);
      token.end_offset = end;
      offset = end;
    } else if (current_char == '"') {
      gsize end = gstring_next_offset(text, offset);
      gboolean escaped = FALSE;
      gboolean found_end = FALSE;
      while (end < len) {
        gunichar c = gstring_get_char(text, end);
        if (escaped)
          escaped = FALSE;
        else if (c == '\\')
          escaped = TRUE;
        else if (c == '"') {
          found_end = TRUE;
          end = gstring_next_offset(text, end);
          break;
        }
        end = gstring_next_offset(text, end);
      }
      token.type = found_end ? LISP_TOKEN_TYPE_STRING : LISP_TOKEN_TYPE_INCOMPLETE_STRING;
      token.end_offset = end;
      offset = end;
    } else {
      token.type = LISP_TOKEN_TYPE_SYMBOL;
      gsize end = offset;
      while (end < len) {
        gunichar c = gstring_get_char(text, end);
        if (g_unichar_isspace(c) || c == '(' || c == ')' || c == '"' || c == ';' || c == ':')
          break;
        end = gstring_next_offset(text, end);
      }
      token.end_offset = end;
      offset = end;
    }

    token.text = gstring_slice_dup(text, token.start_offset, token.end_offset);
    if (token.type == LISP_TOKEN_TYPE_SYMBOL) {
      gchar *endptr = NULL;
      g_ascii_strtod(token.text, &endptr);
      if (endptr && *endptr == '\0')
        token.type = LISP_TOKEN_TYPE_NUMBER;
    }
    g_array_append_val(tokens, token);
  }

  return tokens;
}
