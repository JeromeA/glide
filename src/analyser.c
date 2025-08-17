#include "analyser.h"
#include "node.h"

static void analyse_node(Node *node) {
  if (!node)
    return;

  if (node->type == LISP_AST_NODE_TYPE_LIST && node->children) {
    if (node->children->len > 0) {
      Node *first = g_array_index(node->children, Node*, 0);
      if (first->type == LISP_AST_NODE_TYPE_SYMBOL) {
        if (!first->sd_type)
          node_set_sd_type(first, SDT_FUNCTION_USE);
        if (first->start_token && first->start_token->text &&
            g_ascii_strcasecmp(first->start_token->text, "defun") == 0 &&
            node->children->len > 1) {
          Node *name = g_array_index(node->children, Node*, 1);
          if (name->type == LISP_AST_NODE_TYPE_SYMBOL && !name->sd_type)
            node_set_sd_type(name, SDT_FUNCTION_DEF);
        }
      }
    }
    for (guint i = 0; i < node->children->len; i++)
      analyse_node(g_array_index(node->children, Node*, i));
  }
}

void analyse_ast(Node *root) {
  analyse_node(root);
}

