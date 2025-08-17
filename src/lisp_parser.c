#include <glib.h>
#include "lisp_parser.h"

struct _LispParser {
  Node *ast; /* owns AST */
};

static void lisp_ast_node_free(Node *node);
static void lisp_parser_clear_ast(LispParser *parser);
static Node *parse_expression(GArray *tokens, guint *position);
static Node *parse_symbol(GArray *tokens, guint *position);

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

void lisp_parser_parse(LispParser *parser, GArray *tokens) {
  g_return_if_fail(parser != NULL);
  guint n_tokens = tokens ? tokens->len : 0;

  lisp_parser_clear_ast(parser);

  parser->ast = g_new0(Node, 1);
  g_atomic_int_set(&parser->ast->ref, 1);
  parser->ast->type = LISP_AST_NODE_TYPE_LIST;
  parser->ast->children = g_array_new(FALSE, FALSE, sizeof(Node*));

  guint position = 0;
  while (position < n_tokens) {
    const LispToken *token = &g_array_index(tokens, LispToken, position);
    if (token->type == LISP_TOKEN_TYPE_WHITESPACE || token->type == LISP_TOKEN_TYPE_COMMENT) {
      position++;
      continue;
    }
    Node *expr = parse_expression(tokens, &position);
    if (expr)
      g_array_append_val(parser->ast->children, expr);
  }
}

static Node *parse_symbol(GArray *tokens, guint *position) {
  guint n_tokens = tokens ? tokens->len : 0;
  const LispToken *token = &g_array_index(tokens, LispToken, *position);
  Node *sym = g_new0(Node, 1);
  g_atomic_int_set(&sym->ref, 1);
  sym->type = LISP_AST_NODE_TYPE_SYMBOL;
  sym->children = g_array_new(FALSE, FALSE, sizeof(Node*));
  sym->start_token = token;

  if (token->type == LISP_TOKEN_TYPE_SYMBOL) {
    const LispToken *next = (*position + 1 < n_tokens) ? &g_array_index(tokens, LispToken, *position + 1) : NULL;
    if (next && next->type == LISP_TOKEN_TYPE_SYMBOL_SEPARATOR) {
      Node *pkg = g_new0(Node, 1);
      g_atomic_int_set(&pkg->ref, 1);
      pkg->type = LISP_AST_NODE_TYPE_SYMBOL_PACKAGE;
      pkg->start_token = token;
      pkg->end_token = token;
      g_array_append_val(sym->children, pkg);
      (*position)++;
      const LispToken *sep_tok = &g_array_index(tokens, LispToken, *position);
      Node *sep = g_new0(Node, 1);
      g_atomic_int_set(&sep->ref, 1);
      sep->type = LISP_AST_NODE_TYPE_SYMBOL_SEPARATOR;
      sep->start_token = sep_tok;
      sep->end_token = sep_tok;
      g_array_append_val(sym->children, sep);
      sym->end_token = sep_tok;
      (*position)++;
      if (*position < n_tokens) {
        const LispToken *name_tok = &g_array_index(tokens, LispToken, *position);
        if (name_tok->type == LISP_TOKEN_TYPE_SYMBOL) {
          Node *name = g_new0(Node, 1);
          g_atomic_int_set(&name->ref, 1);
          name->type = LISP_AST_NODE_TYPE_SYMBOL_NAME;
          name->start_token = name_tok;
          name->end_token = name_tok;
          g_array_append_val(sym->children, name);
          sym->end_token = name_tok;
          (*position)++;
        }
      }
    } else {
      Node *name = g_new0(Node, 1);
      g_atomic_int_set(&name->ref, 1);
      name->type = LISP_AST_NODE_TYPE_SYMBOL_NAME;
      name->start_token = token;
      name->end_token = token;
      g_array_append_val(sym->children, name);
      sym->end_token = token;
      (*position)++;
    }
  } else if (token->type == LISP_TOKEN_TYPE_SYMBOL_SEPARATOR) {
    Node *sep = g_new0(Node, 1);
    g_atomic_int_set(&sep->ref, 1);
    sep->type = LISP_AST_NODE_TYPE_SYMBOL_SEPARATOR;
    sep->start_token = token;
    sep->end_token = token;
    g_array_append_val(sym->children, sep);
    sym->end_token = token;
    (*position)++;
    if (*position < n_tokens) {
      const LispToken *name_tok = &g_array_index(tokens, LispToken, *position);
      if (name_tok->type == LISP_TOKEN_TYPE_SYMBOL) {
        Node *name = g_new0(Node, 1);
        g_atomic_int_set(&name->ref, 1);
        name->type = LISP_AST_NODE_TYPE_SYMBOL_NAME;
        name->start_token = name_tok;
        name->end_token = name_tok;
        g_array_append_val(sym->children, name);
        sym->end_token = name_tok;
        (*position)++;
      }
    }
  } else {
    (*position)++;
  }

  return sym;
}

static Node *parse_expression(GArray *tokens, guint *position) {
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
    Node *list_node = g_new0(Node, 1);
    g_atomic_int_set(&list_node->ref, 1);
    list_node->type = LISP_AST_NODE_TYPE_LIST;
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
      Node *child_expr = parse_expression(tokens, position);
      if (child_expr)
        g_array_append_val(list_node->children, child_expr);
    }
    list_node->end_token = NULL;
    return list_node;
  } else if (token->type == LISP_TOKEN_TYPE_NUMBER || token->type == LISP_TOKEN_TYPE_STRING) {
    Node *atom_node = g_new0(Node, 1);
    g_atomic_int_set(&atom_node->ref, 1);
    if (token->type == LISP_TOKEN_TYPE_STRING)
      atom_node->type = LISP_AST_NODE_TYPE_STRING;
    else
      atom_node->type = LISP_AST_NODE_TYPE_NUMBER;
    atom_node->start_token = token;
    atom_node->end_token = token;
    (*position)++;
    return atom_node;
  } else if (token->type == LISP_TOKEN_TYPE_SYMBOL || token->type == LISP_TOKEN_TYPE_SYMBOL_SEPARATOR) {
    return parse_symbol(tokens, position);
  } else if (token->type == LISP_TOKEN_TYPE_LIST_END) {
    (*position)++;
    return NULL;
  }
  (*position)++;
  return NULL;
}

