#include "analyser.h"
#include "node.h"
#include "defpackage.h"
#include <string.h>

static void analyse_node(Project *project, Node *node, gchar **context) {
  if (!node)
    return;

  if (node->type == LISP_AST_NODE_TYPE_LIST && node->children) {
    if (node->children->len > 0) {
      Node *first = g_array_index(node->children, Node*, 0);
      if (first->type == LISP_AST_NODE_TYPE_SYMBOL) {
        const gchar *name = node_get_name(first);
        if (name) {
          if (!first->sd_type)
            node_set_sd_type(first, SDT_FUNCTION_USE, *context);
          if (strcmp(name, "DEFUN") == 0 && node->children->len > 1) {
            Node *fn_name = g_array_index(node->children, Node*, 1);
            if (fn_name->type == LISP_AST_NODE_TYPE_SYMBOL && !fn_name->sd_type)
              node_set_sd_type(fn_name, SDT_FUNCTION_DEF, *context);
          } else if (strcmp(name, "IN-PACKAGE") == 0 && node->children->len > 1) {
            Node *pkg_node = g_array_index(node->children, Node*, 1);
            const gchar *pkg_name = node_get_name(pkg_node);
            if (pkg_name) {
              g_free(*context);
              *context = g_strdup(pkg_name);
            }
          } else if (strcmp(name, "DEFPACKAGE") == 0) {
            analyse_defpackage(project, node, *context);
            return;
          }
        }
      }
    }
    for (guint i = 0; i < node->children->len; i++) {
      Node *child = g_array_index(node->children, Node*, i);
      if (i > 0 && child->type == LISP_AST_NODE_TYPE_SYMBOL && !child->sd_type)
        node_set_sd_type(child, SDT_VAR_USE, *context);
      analyse_node(project, child, context);
    }
  }
}

void analyse_ast(Project *project, Node *root) {
  gchar *context = g_strdup("COMMON-LISP-USER");
  analyse_node(project, root, &context);
  g_free(context);
}

