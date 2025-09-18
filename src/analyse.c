#include "analyse.h"
#include "node.h"
#include "analyse_defpackage.h"
#include "analyse_defun.h"
#include "project.h"
#include "project_file.h"
#include "function.h"
#include <string.h>

static void analyse_mark_error(Node *node) {
  if (!node || !node->file)
    return;
  gsize start = node_get_start_offset(node);
  gsize end = node_get_end_offset(node);
  if (end <= start)
    return;
  project_file_add_error(node->file, start, end);
}

static const gchar *analyse_get_symbol_name(const Node *node) {
  if (!node)
    return NULL;
  if (node->type == LISP_AST_NODE_TYPE_SYMBOL) {
    const Node *name = node_get_symbol_name_node_const(node);
    return name ? node_get_name(name) : NULL;
  }
  if (node->type == LISP_AST_NODE_TYPE_SYMBOL_NAME)
    return node_get_name(node);
  return NULL;
}

static gboolean analyse_lambda_arity(const Node *lambda, guint *min_args,
    guint *max_args, gboolean *has_max) {
  g_return_val_if_fail(min_args != NULL, FALSE);
  g_return_val_if_fail(max_args != NULL, FALSE);
  g_return_val_if_fail(has_max != NULL, FALSE);
  g_return_val_if_fail(lambda != NULL, FALSE);
  g_return_val_if_fail(lambda->type == LISP_AST_NODE_TYPE_LIST, FALSE);
  if (!lambda->children || lambda->children->len == 0) {
    *min_args = 0;
    *max_args = 0;
    *has_max = TRUE;
    return TRUE;
  }
  guint min_count = 0;
  guint max_count = 0;
  gboolean max_known = TRUE;
  gboolean optional = FALSE;
  gboolean skip_next = FALSE;
  for (guint i = 0; i < lambda->children->len; i++) {
    const Node *param = g_array_index(lambda->children, Node*, i);
    if (skip_next) {
      skip_next = FALSE;
      continue;
    }
    const gchar *name = analyse_get_symbol_name(param);
    if (name && name[0] == '&') {
      if (strcmp(name, "&OPTIONAL") == 0) {
        optional = TRUE;
        continue;
      }
      if (strcmp(name, "&REST") == 0 || strcmp(name, "&BODY") == 0) {
        max_known = FALSE;
        skip_next = TRUE;
        optional = FALSE;
        continue;
      }
      if (strcmp(name, "&KEY") == 0 || strcmp(name, "&ALLOW-OTHER-KEYS") == 0) {
        max_known = FALSE;
        optional = TRUE;
        continue;
      }
      if (strcmp(name, "&AUX") == 0) {
        break;
      }
      if (strcmp(name, "&ENVIRONMENT") == 0 || strcmp(name, "&WHOLE") == 0) {
        skip_next = TRUE;
        continue;
      }
      continue;
    }
    if (optional) {
      if (max_known)
        max_count++;
      continue;
    }
    min_count++;
    if (max_known)
      max_count++;
  }
  *min_args = min_count;
  *max_args = max_count;
  *has_max = max_known;
  return TRUE;
}

static gboolean analyse_validate_call(Project *project, Node *expr) {
  if (!expr || !expr->children || expr->children->len == 0)
    return TRUE;
  Node *head = g_array_index(expr->children, Node*, 0);
  const gchar *fn_name = analyse_get_symbol_name(head);
  if (!fn_name)
    return TRUE;
  Function *function = project_get_function(project, fn_name);
  if (!function)
    return TRUE;
  const Node *lambda = function_get_lambda_list(function);
  guint min_args = 0;
  guint max_args = 0;
  gboolean has_max = TRUE;
  if (!analyse_lambda_arity(lambda, &min_args, &max_args, &has_max))
    return TRUE;
  guint actual = expr->children->len > 0 ? expr->children->len - 1 : 0;
  if (actual < min_args || (has_max && actual > max_args)) {
    analyse_mark_error(expr);
    return FALSE;
  }
  return TRUE;
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
          gboolean call_valid = context->backquote || analyse_validate_call(project, node);
          if (!context->backquote && call_valid) {
            if (strcmp(name, "DEFUN") == 0) {
              analyse_defun(project, node, context);
              return;
            } else if (strcmp(name, "IN-PACKAGE") == 0 && node->children->len > 1) {
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

