#include "analyse.h"
#include "node.h"
#include "analyse_defpackage.h"
#include "analyse_defun.h"
#include "project.h"
#include "document.h"
#include "function.h"
#include "analyse_call.h"
#include <string.h>

static void analyse_diagnose_call(Project *project, Node *expr) {
  DocumentError error = { 0 };
  Node *target = NULL;
  if (analyse_call_check(project, expr, &error, &target))
    return;
  Node *err_node = target ? target : expr;
  if (!err_node || !err_node->document || error.end <= error.start) {
    g_free(error.message);
    return;
  }
  document_add_error(err_node->document, error);
  g_free(error.message);
}

void analyse_node(Project *project, Node *node, AnalyseContext *context) {
  if (!node)
    return;

  if (node->type == LISP_AST_NODE_TYPE_BACKQUOTE && node->children) {
    gboolean prev = context->backquote;
    context->backquote = TRUE;
    for (guint i = 0; i < node->children->len; i++)
      analyse_node(project, g_array_index(node->children, Node*, i), context);
    context->backquote = prev;
    return;
  }

  if ((node->type == LISP_AST_NODE_TYPE_UNQUOTE ||
       node->type == LISP_AST_NODE_TYPE_UNQUOTE_SPLICING) && node->children) {
    gboolean prev = context->backquote;
    context->backquote = FALSE;
    for (guint i = 0; i < node->children->len; i++)
      analyse_node(project, g_array_index(node->children, Node*, i), context);
    context->backquote = prev;
    return;
  }

  if (node->type == LISP_AST_NODE_TYPE_LIST && node->children) {
    if (node->children->len > 0) {
      Node *first = g_array_index(node->children, Node*, 0);
      if (first->type == LISP_AST_NODE_TYPE_SYMBOL) {
        Node *first_name = node_get_symbol_name_node(first);
        const gchar *name = node_get_name(first);
        if (name) {
          if (first_name && !first_name->sd_type)
            node_set_sd_type(first_name, SDT_FUNCTION_USE, context->package);
          if (!context->backquote) {
            if (strcmp(name, "DEFUN") == 0) {
              analyse_defun(project, node, context);
              return;
            }
            if (strcmp(name, "IN-PACKAGE") == 0 && node->children->len > 1) {
              Node *pkg_node = g_array_index(node->children, Node*, 1);
              analyse_node(project, pkg_node, context);
              const gchar *pkg_name = node_get_name(pkg_node);
              if (pkg_name) {
                g_free(context->package);
                context->package = g_strdup(pkg_name);
              }
            } else if (strcmp(name, "DEFPACKAGE") == 0) {
              analyse_defpackage(project, node, context);
              return;
            } else {
              analyse_diagnose_call(project, node);
            }
          }
        }
      }
    }
    for (guint i = 0; i < node->children->len; i++) {
      Node *child = g_array_index(node->children, Node*, i);
      if (i > 0 && child->type == LISP_AST_NODE_TYPE_SYMBOL) {
        Node *child_name = node_get_symbol_name_node(child);
        if (child_name && !child_name->sd_type)
          node_set_sd_type(child_name, SDT_VAR_USE, context->package);
      }
      analyse_node(project, child, context);
    }
  }
}

void analyse_ast(Project *project, Node *root) {
  g_return_if_fail(root != NULL);

  AnalyseContext context = { g_strdup("CL-USER"), FALSE };
  if (root->children) {
    for (guint i = 0; i < root->children->len; i++)
      analyse_node(project, g_array_index(root->children, Node*, i), &context);
  }
  g_free(context.package);
}

