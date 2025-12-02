#include <glib.h>
#include "lisp_parser.h"

typedef struct {
  GHashTable *nodes_by_start_offset; /* gsize -> Node* */
  GHashTable *tokens_present; /* LispToken* -> gpointer */
  gsize change_start;
  gsize change_end;
} ReuseContext;

static Node *parse_expression(Document *document, GArray *tokens, guint *position,
                              ReuseContext *reuse_context);
static Node *parse_symbol(Document *document, GArray *tokens, guint *position);

static void reuse_context_collect_node(Node *node, ReuseContext *reuse_context) {
  if (!node || !reuse_context)
    return;

  if (node->start_token && node->end_token) {
    if (!reuse_context->tokens_present ||
        !g_hash_table_contains(reuse_context->tokens_present, node->start_token) ||
        !g_hash_table_contains(reuse_context->tokens_present, node->end_token))
      goto recurse_children;

    gsize start_offset = node_get_start_offset(node);
    gsize end_offset = node_get_end_offset(node);
    gboolean outside_change = (end_offset <= reuse_context->change_start) ||
                              (start_offset >= reuse_context->change_end);
    if (outside_change) {
      Node *existing = g_hash_table_lookup(reuse_context->nodes_by_start_offset,
                                           GUINT_TO_POINTER(start_offset));
      if (!existing || node_get_end_offset(existing) < end_offset)
        g_hash_table_insert(reuse_context->nodes_by_start_offset,
                            GUINT_TO_POINTER(start_offset), node);
    }
  }

recurse_children:
  if (node->children) {
    for (guint i = 0; i < node->children->len; i++) {
      Node *child = g_array_index(node->children, Node*, i);
      reuse_context_collect_node(child, reuse_context);
    }
  }
}

static void reuse_context_init(ReuseContext *reuse_context, Node *previous_ast,
                               GArray *tokens, gsize change_start, gsize change_end) {
  reuse_context->nodes_by_start_offset = NULL;
  reuse_context->tokens_present = NULL;
  reuse_context->change_start = change_start;
  reuse_context->change_end = change_end;
  if (!previous_ast)
    return;

  reuse_context->nodes_by_start_offset = g_hash_table_new(g_direct_hash, g_direct_equal);
  if (tokens) {
    reuse_context->tokens_present = g_hash_table_new(g_direct_hash, g_direct_equal);
    for (guint i = 0; i < tokens->len; i++) {
      LispToken *token = &g_array_index(tokens, LispToken, i);
      g_hash_table_add(reuse_context->tokens_present, token);
    }
  }
  reuse_context_collect_node(previous_ast, reuse_context);
}

static void reuse_context_clear(ReuseContext *reuse_context) {
  if (!reuse_context)
    return;
  if (reuse_context->tokens_present)
    g_hash_table_destroy(reuse_context->tokens_present);
  if (reuse_context->nodes_by_start_offset)
    g_hash_table_destroy(reuse_context->nodes_by_start_offset);
  reuse_context->nodes_by_start_offset = NULL;
  reuse_context->tokens_present = NULL;
}

static void detach_reused_subtree(Node *node) {
  if (!node || !node->parent || !node->parent->children)
    return;

  GArray *siblings = node->parent->children;
  for (guint i = 0; i < siblings->len; i++) {
    Node *sibling = g_array_index(siblings, Node*, i);
    if (sibling == node) {
      g_array_index(siblings, Node*, i) = NULL;
      break;
    }
  }
}

static Node *try_reuse_node(GArray *tokens, guint *position, ReuseContext *reuse_context) {
  if (!reuse_context || !reuse_context->nodes_by_start_offset || !tokens)
    return NULL;

  const LispToken *token = &g_array_index(tokens, LispToken, *position);
  gsize token_offset = marker_get_offset(token->start_marker);
  Node *candidate = g_hash_table_lookup(reuse_context->nodes_by_start_offset,
                                        GUINT_TO_POINTER(token_offset));
  if (!candidate)
    return NULL;

  if (!candidate->end_token)
    return NULL;

  if (candidate->start_token != token)
    return NULL;

  gsize candidate_start = node_get_start_offset(candidate);
  gsize candidate_end = node_get_end_offset(candidate);
  gboolean overlaps_change = !(candidate_end <= reuse_context->change_start ||
                               candidate_start >= reuse_context->change_end);
  if (overlaps_change)
    return NULL;

  guint end_index = *position;
  while (end_index < tokens->len) {
    const LispToken *current_token = &g_array_index(tokens, LispToken, end_index);
    end_index++;
    if (current_token == candidate->end_token)
      break;
  }

  if (end_index <= *position || end_index > tokens->len)
    return NULL;

  detach_reused_subtree(candidate);
  *position = end_index;
  return candidate;
}

Node *lisp_parser_parse(GArray *tokens, Document *document, Node *previous_ast,
                       gsize change_start, gsize change_end) {
  Node *ast = node_new(LISP_AST_NODE_TYPE_LIST, document);
  ast->children = g_array_new(FALSE, FALSE, sizeof(Node*));

  guint n_tokens = tokens ? tokens->len : 0;
  guint position = 0;
  ReuseContext reuse_context;
  reuse_context_init(&reuse_context, previous_ast, tokens, change_start, change_end);
  while (position < n_tokens) {
    const LispToken *token = &g_array_index(tokens, LispToken, position);
    if (token->type == LISP_TOKEN_TYPE_WHITESPACE || token->type == LISP_TOKEN_TYPE_COMMENT) {
      position++;
      continue;
    }
    Node *expr = parse_expression(document, tokens, &position, &reuse_context);
    if (expr) {
      expr->parent = ast;
      g_array_append_val(ast->children, expr);
    }
  }

  reuse_context_clear(&reuse_context);

  return ast;
}

static Node *parse_symbol(Document *document, GArray *tokens, guint *position) {
  guint n_tokens = tokens ? tokens->len : 0;
  const LispToken *token = &g_array_index(tokens, LispToken, *position);
  Node *sym = node_new(LISP_AST_NODE_TYPE_SYMBOL, document);
  sym->children = g_array_new(FALSE, FALSE, sizeof(Node*));
  sym->start_token = token;

  if (token->type == LISP_TOKEN_TYPE_SYMBOL) {
    const LispToken *next = (*position + 1 < n_tokens) ? &g_array_index(tokens, LispToken, *position + 1) : NULL;
    if (next && next->type == LISP_TOKEN_TYPE_SYMBOL_SEPARATOR) {
      Node *pkg = node_new(LISP_AST_NODE_TYPE_SYMBOL_PACKAGE, document);
      pkg->start_token = token;
      pkg->end_token = token;
      g_array_append_val(sym->children, pkg);
      pkg->parent = sym;
      (*position)++;

      const LispToken *sep_tok = &g_array_index(tokens, LispToken, *position);
      Node *sep = node_new(LISP_AST_NODE_TYPE_SYMBOL_SEPARATOR, document);
      sep->start_token = sep_tok;
      sep->end_token = sep_tok;
      g_array_append_val(sym->children, sep);
      sep->parent = sym;
      sym->end_token = sep_tok;
      (*position)++;

      if (*position < n_tokens) {
        const LispToken *name_tok = &g_array_index(tokens, LispToken, *position);
        if (name_tok->type == LISP_TOKEN_TYPE_SYMBOL) {
          Node *name = node_new(LISP_AST_NODE_TYPE_SYMBOL_NAME, document);
          name->start_token = name_tok;
          name->end_token = name_tok;
          g_array_append_val(sym->children, name);
          name->parent = sym;
          sym->end_token = name_tok;
          (*position)++;
        }
      }
    } else {
      Node *name = node_new(LISP_AST_NODE_TYPE_SYMBOL_NAME, document);
      name->start_token = token;
      name->end_token = token;
      g_array_append_val(sym->children, name);
      name->parent = sym;
      sym->end_token = token;
      (*position)++;
    }
  } else if (token->type == LISP_TOKEN_TYPE_SYMBOL_SEPARATOR) {
    Node *sep = node_new(LISP_AST_NODE_TYPE_SYMBOL_SEPARATOR, document);
    sep->start_token = token;
    sep->end_token = token;
    g_array_append_val(sym->children, sep);
    sep->parent = sym;
    sym->end_token = token;
    (*position)++;

    if (*position < n_tokens) {
      const LispToken *name_tok = &g_array_index(tokens, LispToken, *position);
      if (name_tok->type == LISP_TOKEN_TYPE_SYMBOL) {
        Node *name = node_new(LISP_AST_NODE_TYPE_SYMBOL_NAME, document);
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

static Node *parse_expression(Document *document, GArray *tokens, guint *position,
                              ReuseContext *reuse_context) {
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

  Node *reused = try_reuse_node(tokens, position, reuse_context);
  if (reused)
    return reused;

  if (token->type == LISP_TOKEN_TYPE_LIST_START) {
    Node *list_node = node_new(LISP_AST_NODE_TYPE_LIST, document);
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
      Node *child_expr = parse_expression(document, tokens, position, reuse_context);
      if (child_expr) {
        child_expr->parent = list_node;
        g_array_append_val(list_node->children, child_expr);
      }
    }
    list_node->end_token = NULL;
    return list_node;
  } else if (token->type == LISP_TOKEN_TYPE_QUOTE ||
             token->type == LISP_TOKEN_TYPE_BACKQUOTE ||
             token->type == LISP_TOKEN_TYPE_UNQUOTE ||
             token->type == LISP_TOKEN_TYPE_UNQUOTE_SPLICING) {
    LispAstNodeType type;
    switch(token->type) {
      case LISP_TOKEN_TYPE_QUOTE:
        type = LISP_AST_NODE_TYPE_QUOTE;
        break;
      case LISP_TOKEN_TYPE_BACKQUOTE:
        type = LISP_AST_NODE_TYPE_BACKQUOTE;
        break;
      case LISP_TOKEN_TYPE_UNQUOTE:
        type = LISP_AST_NODE_TYPE_UNQUOTE;
        break;
      default:
        type = LISP_AST_NODE_TYPE_UNQUOTE_SPLICING;
        break;
    }
    Node *macro_node = node_new(type, document);
    macro_node->start_token = token;
    macro_node->children = g_array_new(FALSE, FALSE, sizeof(Node*));
    (*position)++;
    Node *child_expr = parse_expression(document, tokens, position, reuse_context);
    if (child_expr) {
      child_expr->parent = macro_node;
      g_array_append_val(macro_node->children, child_expr);
      macro_node->end_token = child_expr->end_token;
    } else {
      macro_node->end_token = token;
    }
    return macro_node;
  } else if (token->type == LISP_TOKEN_TYPE_NUMBER || token->type == LISP_TOKEN_TYPE_STRING) {
    Node *atom_node;
    if (token->type == LISP_TOKEN_TYPE_STRING)
      atom_node = node_new(LISP_AST_NODE_TYPE_STRING, document);
    else
      atom_node = node_new(LISP_AST_NODE_TYPE_NUMBER, document);
    atom_node->start_token = token;
    atom_node->end_token = token;
    (*position)++;
    return atom_node;
  } else if (token->type == LISP_TOKEN_TYPE_SYMBOL || token->type == LISP_TOKEN_TYPE_SYMBOL_SEPARATOR) {
    return parse_symbol(document, tokens, position);
  } else if (token->type == LISP_TOKEN_TYPE_LIST_END) {
    (*position)++;
    return NULL;
  }
  (*position)++;
  return NULL;
}

