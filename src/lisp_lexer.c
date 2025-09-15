// Ensure all dependency headers are processed before our own header.
#include <glib.h>
#include "lisp_lexer.h"

struct _LispLexer {
  TextProvider *provider; /* not owned */
  GArray *tokens; /* owns LispToken */
};

static void lisp_token_free(gpointer token);
static void lisp_lexer_clear_tokens(LispLexer *lexer);

static void lisp_token_free(gpointer token) {
  if (!token) return;
  LispToken *t = token;
  g_free(t->text);
}

static void lisp_lexer_clear_tokens(LispLexer *lexer) {
  if (lexer->tokens) {
    g_array_free(lexer->tokens, TRUE);
    lexer->tokens = NULL;
  }
}

LispLexer *lisp_lexer_new(TextProvider *provider) {
  g_return_val_if_fail(provider, NULL);
  LispLexer *lexer = g_new0(LispLexer, 1);
  lexer->provider = provider;
  return lexer;
}

void lisp_lexer_free(LispLexer *lexer) {
  g_return_if_fail(lexer != NULL);
  lisp_lexer_clear_tokens(lexer);
  g_free(lexer);
}

void lisp_lexer_lex(LispLexer *lexer) {
  g_return_if_fail(lexer != NULL);
  g_return_if_fail(lexer->provider);

  lisp_lexer_clear_tokens(lexer);
  lexer->tokens = g_array_new(FALSE, TRUE, sizeof(LispToken));
  g_array_set_clear_func(lexer->tokens, lisp_token_free);

  gsize len = text_provider_get_length(lexer->provider);
  gsize offset = 0;

  while (offset < len) {
    gunichar current_char = text_provider_get_char(lexer->provider, offset);
    LispToken token = {0};
    token.start_offset = offset;

    if (g_unichar_isspace(current_char)) {
      token.type = LISP_TOKEN_TYPE_WHITESPACE;
      gsize end = offset;
      while (end < len && g_unichar_isspace(text_provider_get_char(lexer->provider, end)))
        end = text_provider_next_offset(lexer->provider, end);
      token.end_offset = end;
      offset = end;
    } else if (current_char == ';') {
      token.type = LISP_TOKEN_TYPE_COMMENT;
      gsize end = offset;
      while (end < len && text_provider_get_char(lexer->provider, end) != '\n')
        end = text_provider_next_offset(lexer->provider, end);
      token.end_offset = end;
      offset = end;
    } else if (current_char == '(') {
      token.type = LISP_TOKEN_TYPE_LIST_START;
      offset = text_provider_next_offset(lexer->provider, offset);
      token.end_offset = offset;
    } else if (current_char == ')') {
      token.type = LISP_TOKEN_TYPE_LIST_END;
      offset = text_provider_next_offset(lexer->provider, offset);
      token.end_offset = offset;
    } else if (current_char == 0x27) {
      token.type = LISP_TOKEN_TYPE_QUOTE;
      offset = text_provider_next_offset(lexer->provider, offset);
      token.end_offset = offset;
    } else if (current_char == '`') {
      token.type = LISP_TOKEN_TYPE_BACKQUOTE;
      offset = text_provider_next_offset(lexer->provider, offset);
      token.end_offset = offset;
    } else if (current_char == ',') {
      gsize end = text_provider_next_offset(lexer->provider, offset);
      if (end < len && text_provider_get_char(lexer->provider, end) == '@') {
        end = text_provider_next_offset(lexer->provider, end);
        token.type = LISP_TOKEN_TYPE_UNQUOTE_SPLICING;
      } else {
        token.type = LISP_TOKEN_TYPE_UNQUOTE;
      }
      token.end_offset = end;
      offset = end;
    } else if (current_char == ':') {
      token.type = LISP_TOKEN_TYPE_SYMBOL_SEPARATOR;
      gsize end = text_provider_next_offset(lexer->provider, offset);
      if (end < len && text_provider_get_char(lexer->provider, end) == ':')
        end = text_provider_next_offset(lexer->provider, end);
      token.end_offset = end;
      offset = end;
    } else if (current_char == '"') {
      gsize end = text_provider_next_offset(lexer->provider, offset);
      gboolean escaped = FALSE;
      gboolean found_end = FALSE;
      while (end < len) {
        gunichar c = text_provider_get_char(lexer->provider, end);
        if (escaped)
          escaped = FALSE;
        else if (c == '\\')
          escaped = TRUE;
        else if (c == '"') {
          found_end = TRUE;
          end = text_provider_next_offset(lexer->provider, end);
          break;
        }
        end = text_provider_next_offset(lexer->provider, end);
      }
      token.type = found_end ? LISP_TOKEN_TYPE_STRING : LISP_TOKEN_TYPE_INCOMPLETE_STRING;
      token.end_offset = end;
      offset = end;
    } else {
      token.type = LISP_TOKEN_TYPE_SYMBOL;
      gsize end = offset;
      while (end < len) {
        gunichar c = text_provider_get_char(lexer->provider, end);
        if (g_unichar_isspace(c) || c == '(' || c == ')' || c == '"' || c == ';' || c == ':')
          break;
        end = text_provider_next_offset(lexer->provider, end);
      }
      token.end_offset = end;
      offset = end;
    }

    token.text = text_provider_get_text(lexer->provider, token.start_offset, token.end_offset);
    if (token.type == LISP_TOKEN_TYPE_SYMBOL) {
      gchar *endptr = NULL;
      g_ascii_strtod(token.text, &endptr);
      if (endptr && *endptr == '\0')
        token.type = LISP_TOKEN_TYPE_NUMBER;
    }
    g_array_append_val(lexer->tokens, token);
  }
}

GArray *lisp_lexer_get_tokens(LispLexer *lexer) {
  g_return_val_if_fail(lexer != NULL, NULL);
  return lexer->tokens;
}
