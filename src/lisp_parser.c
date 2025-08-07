// Ensure all dependency headers are processed before our own header.
#include <glib.h>
#include "lisp_parser.h"

struct _LispParser {
  LispAstNode *ast; /* owns AST */
};

static void lisp_ast_node_free(LispAstNode *node);
static void lisp_parser_clear_ast(LispParser *parser);
static LispAstNode *parse_expression(GArray *tokens, guint *position);

static void lisp_ast_node_free(LispAstNode *node) {
  if (!node) return;
  if (node->children) {
    for (guint i = 0; i < node->children->len; i++)
      lisp_ast_node_free(g_array_index(node->children, LispAstNode*, i));
    g_array_free(node->children, TRUE);
  }
  g_free(node);
}

static void lisp_parser_clear_ast(LispParser *parser) {
  if (parser->ast) {
    lisp_ast_node_free(parser->ast);
    parser->ast = NULL;
  }
}

LispParser *lisp_parser_new(void) {
  return g_new0(LispParser, 1);
}

void lisp_parser_free(LispParser *parser) {
  g_return_if_fail(parser != NULL);
  lisp_parser_clear_ast(parser);
  g_free(parser);
}

const LispAstNode *lisp_parser_get_ast(LispParser *parser) {
  g_return_val_if_fail(parser != NULL, NULL);
  return parser->ast;
}

void lisp_parser_parse(LispParser *parser, GArray *tokens) {
  g_return_if_fail(parser != NULL);
  guint n_tokens = tokens ? tokens->len : 0;

  lisp_parser_clear_ast(parser);

  parser->ast = g_new0(LispAstNode, 1);
  parser->ast->type = LISP_AST_NODE_TYPE_LIST;
  parser->ast->children = g_array_new(FALSE, FALSE, sizeof(LispAstNode*));

  guint position = 0;
  while (position < n_tokens) {
    const LispToken *token = &g_array_index(tokens, LispToken, position);
    if (token->type == LISP_TOKEN_TYPE_WHITESPACE || token->type == LISP_TOKEN_TYPE_COMMENT) {
      position++;
      continue;
    }
    LispAstNode *expr = parse_expression(tokens, &position);
    if (expr)
      g_array_append_val(parser->ast->children, expr);
  }
}

static LispAstNode *parse_expression(GArray *tokens, guint *position) {
  guint n_tokens = tokens ? tokens->len : 0;
  while (*position < n_tokens) {
    const LispToken *token = &g_array_index(tokens, LispToken, *position);
    if (token->type != LISP_TOKEN_TYPE_WHITESPACE && token->type != LISP_TOKEN_TYPE_COMMENT)
      break;
    (*position)++;
  }

  if (*position >= n_tokens)
    return NULL;

  const LispToken *token = &g_array_index(tokens, LispToken, *position);

  if (token->type == LISP_TOKEN_TYPE_LIST_START) {
    LispAstNode *list_node = g_new0(LispAstNode, 1);
    list_node->type = LISP_AST_NODE_TYPE_LIST;
    list_node->start_token = token;
    list_node->children = g_array_new(FALSE, FALSE, sizeof(LispAstNode*));

    (*position)++;
    while (*position < n_tokens) {
      const LispToken *current_token = &g_array_index(tokens, LispToken, *position);
      if (current_token->type == LISP_TOKEN_TYPE_LIST_END) {
        list_node->end_token = current_token;
        (*position)++;
        return list_node;
      }
      if (current_token->type == LISP_TOKEN_TYPE_WHITESPACE || current_token->type == LISP_TOKEN_TYPE_COMMENT) {
        (*position)++;
        continue;
      }
      LispAstNode *child_expr = parse_expression(tokens, position);
      if (child_expr)
        g_array_append_val(list_node->children, child_expr);
    }
    list_node->end_token = NULL;
    return list_node;
  } else if (token->type == LISP_TOKEN_TYPE_NUMBER || token->type == LISP_TOKEN_TYPE_SYMBOL || token->type == LISP_TOKEN_TYPE_STRING) {
    LispAstNode *atom_node = g_new0(LispAstNode, 1);
    if (token->type == LISP_TOKEN_TYPE_STRING)
      atom_node->type = LISP_AST_NODE_TYPE_STRING;
    else if (token->type == LISP_TOKEN_TYPE_NUMBER)
      atom_node->type = LISP_AST_NODE_TYPE_NUMBER;
    else
      atom_node->type = LISP_AST_NODE_TYPE_SYMBOL;
    atom_node->start_token = token;
    atom_node->end_token = token;
    (*position)++;
    return atom_node;
  } else if (token->type == LISP_TOKEN_TYPE_LIST_END) {
    (*position)++;
    return NULL;
  }
  (*position)++;
  return NULL;
}
