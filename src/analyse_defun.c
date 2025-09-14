#include "analyse_defun.h"
#include "analyse.h"
#include "function.h"

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

  Node *doc_node = NULL;
  if (expr->children->len > 3) {
    Node *maybe_doc = g_array_index(expr->children, Node*, 3);
    if (maybe_doc->type == LISP_AST_NODE_TYPE_STRING)
      doc_node = maybe_doc;
  }
  Function *function = function_new(name_node, args,
      doc_node ? node_get_name(doc_node) : NULL, NULL,
      FUNCTION_KIND_FUNCTION, node_get_name(name_node), *context, NULL);
  if (node_is_toplevel(expr))
    project_add_function(project, function);
  function_unref(function);

  guint start = doc_node ? 4 : 3;
  for (guint i = start; i < expr->children->len; i++) {
    Node *child = g_array_index(expr->children, Node*, i);
    analyse_node(project, child, context);
  }
}

