#include "analyse_defun.h"
#include "analyse.h"

void analyse_defun(Project *project, Node *expr, gchar **context) {
  if (!expr || !expr->children || expr->children->len < 3)
    return;

  Node *name_node = g_array_index(expr->children, Node*, 1);
  if (name_node->type == LISP_AST_NODE_TYPE_SYMBOL && !name_node->sd_type)
    node_set_sd_type(name_node, SDT_FUNCTION_DEF, *context);

  Node *args = g_array_index(expr->children, Node*, 2);
  if (args->type == LISP_AST_NODE_TYPE_LIST && args->children) {
    for (guint i = 0; i < args->children->len; i++) {
      Node *arg = g_array_index(args->children, Node*, i);
      if (arg->type == LISP_AST_NODE_TYPE_SYMBOL && !arg->sd_type)
        node_set_sd_type(arg, SDT_VAR_DEF, *context);
    }
  }

  for (guint i = 3; i < expr->children->len; i++) {
    Node *child = g_array_index(expr->children, Node*, i);
    analyse_node(project, child, context);
  }
}

