#include <glib.h>
#include "lisp_parser.h"

struct _LispParser {
  Node *ast; /* owns AST */
  ProjectFile *file;
};

static void lisp_ast_node_free(Node *node);
static void lisp_parser_clear_ast(LispParser *parser);
static Node *parse_expression(LispParser *parser, GArray *tokens, guint *position);
static Node *parse_symbol(LispParser *parser, GArray *tokens, guint *position);

static void lisp_ast_node_free(Node *node) {
  if (!node) return;
  if (node->children) {
    for (guint i = 0; i < node->children->len; i++)
      lisp_ast_node_free(g_array_index(node->children, Node*, i));
    g_array_free(node->children, TRUE);
    node->children = NULL;
  }
  node_unref(node);
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

const Node *lisp_parser_get_ast(LispParser *parser) {
  g_return_val_if_fail(parser != NULL, NULL);
  return parser->ast;
}

void lisp_parser_parse(LispParser *parser, GArray *tokens, ProjectFile *file) {
  g_return_if_fail(parser != NULL);
  guint n_tokens = tokens ? tokens->len : 0;

  parser->file = file;
  lisp_parser_clear_ast(parser);

  parser->ast = node_new(LISP_AST_NODE_TYPE_LIST, file);
  parser->ast->children = g_array_new(FALSE, FALSE, sizeof(Node*));

  guint position = 0;
  while (position < n_tokens) {
    const LispToken *token = &g_array_index(tokens, LispToken, position);
    if (token->type == LISP_TOKEN_TYPE_WHITESPACE || token->type == LISP_TOKEN_TYPE_COMMENT) {
      position++;
      continue;
    }
    Node *expr = parse_expression(parser, tokens, &position);
    if (expr) {
      expr->parent = parser->ast;
      g_array_append_val(parser->ast->children, expr);
    }
  }
}

static Node *parse_symbol(LispParser *parser, GArray *tokens, guint *position) {
  guint n_tokens = tokens ? tokens->len : 0;
  const LispToken *token = &g_array_index(tokens, LispToken, *position);
  Node *sym = node_new(LISP_AST_NODE_TYPE_SYMBOL, parser->file);
  sym->children = g_array_new(FALSE, FALSE, sizeof(Node*));
  sym->start_token = token;

  if (token->type == LISP_TOKEN_TYPE_SYMBOL) {
    const LispToken *next = (*position + 1 < n_tokens) ? &g_array_index(tokens, LispToken, *position + 1) : NULL;
    if (next && next->type == LISP_TOKEN_TYPE_SYMBOL_SEPARATOR) {
      Node *pkg = node_new(LISP_AST_NODE_TYPE_SYMBOL_PACKAGE, parser->file);
      pkg->start_token = token;
      pkg->end_token = token;
      g_array_append_val(sym->children, pkg);
      pkg->parent = sym;
      (*position)++;
      const LispToken *sep_tok = &g_array_index(tokens, LispToken, *position);
      Node *sep = node_new(LISP_AST_NODE_TYPE_SYMBOL_SEPARATOR, parser->file);
      sep->start_token = sep_tok;
      sep->end_token = sep_tok;
      g_array_append_val(sym->children, sep);
      sep->parent = sym;
      sym->end_token = sep_tok;
      (*position)++;
      if (*position < n_tokens) {
        const LispToken *name_tok = &g_array_index(tokens, LispToken, *position);
        if (name_tok->type == LISP_TOKEN_TYPE_SYMBOL) {
          Node *name = node_new(LISP_AST_NODE_TYPE_SYMBOL_NAME, parser->file);
          name->start_token = name_tok;
          name->end_token = name_tok;
          g_array_append_val(sym->children, name);
          name->parent = sym;
          sym->end_token = name_tok;
          (*position)++;
        }
      }
    } else {
      Node *name = node_new(LISP_AST_NODE_TYPE_SYMBOL_NAME, parser->file);
      name->start_token = token;
      name->end_token = token;
      g_array_append_val(sym->children, name);
      name->parent = sym;
      sym->end_token = token;
      (*position)++;
    }
  } else if (token->type == LISP_TOKEN_TYPE_SYMBOL_SEPARATOR) {
    Node *sep = node_new(LISP_AST_NODE_TYPE_SYMBOL_SEPARATOR, parser->file);
    sep->start_token = token;
    sep->end_token = token;
    g_array_append_val(sym->children, sep);
    sep->parent = sym;
    sym->end_token = token;
    (*position)++;
    if (*position < n_tokens) {
      const LispToken *name_tok = &g_array_index(tokens, LispToken, *position);
      if (name_tok->type == LISP_TOKEN_TYPE_SYMBOL) {
        Node *name = node_new(LISP_AST_NODE_TYPE_SYMBOL_NAME, parser->file);
        name->start_token = name_tok;
        name->end_token = name_tok;
        g_array_append_val(sym->children, name);
        name->parent = sym;
        sym->end_token = name_tok;
        (*position)++;
      }
    }
  } else {
    (*position)++;
  }

  return sym;
}

static Node *parse_expression(LispParser *parser, GArray *tokens, guint *position) {
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
    Node *list_node = node_new(LISP_AST_NODE_TYPE_LIST, parser->file);
    list_node->start_token = token;
    list_node->children = g_array_new(FALSE, FALSE, sizeof(Node*));

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
      Node *child_expr = parse_expression(parser, tokens, position);
      if (child_expr) {
        child_expr->parent = list_node;
        g_array_append_val(list_node->children, child_expr);
      }
    }
    list_node->end_token = NULL;
    return list_node;
  } else if (token->type == LISP_TOKEN_TYPE_NUMBER || token->type == LISP_TOKEN_TYPE_STRING) {
    Node *atom_node;
    if (token->type == LISP_TOKEN_TYPE_STRING)
      atom_node = node_new(LISP_AST_NODE_TYPE_STRING, parser->file);
    else
      atom_node = node_new(LISP_AST_NODE_TYPE_NUMBER, parser->file);
    atom_node->start_token = token;
    atom_node->end_token = token;
    (*position)++;
    return atom_node;
  } else if (token->type == LISP_TOKEN_TYPE_SYMBOL || token->type == LISP_TOKEN_TYPE_SYMBOL_SEPARATOR) {
    return parse_symbol(parser, tokens, position);
  } else if (token->type == LISP_TOKEN_TYPE_LIST_END) {
    (*position)++;
    return NULL;
  }
  (*position)++;
  return NULL;
}

