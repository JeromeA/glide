#include "analyse_call.h"
#include "project.h"
#include "function.h"
#include <string.h>

static const gchar *analyse_call_get_symbol_name(const Node *node) {
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

static gboolean analyse_call_lambda_arity(const Node *lambda, guint *min_args,
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
    const gchar *name = analyse_call_get_symbol_name(param);
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

gboolean analyse_call_check(Project *project, Node *expr,
    DocumentError *error, Node **target) {
  if (target)
    *target = NULL;
  if (error) {
    error->start = 0;
    error->end = 0;
    error->type = DOCUMENT_ERROR_TYPE_GENERIC;
    error->message = NULL;
  }
  if (!expr || !expr->children || expr->children->len == 0)
    return TRUE;
  Node *head = g_array_index(expr->children, Node*, 0);
  const gchar *fn_name = analyse_call_get_symbol_name(head);
  if (!fn_name)
    return TRUE;
  Function *function = project_get_function(project, fn_name);
  if (!function) {
    Node *name_node = node_get_symbol_name_node(head);
    Node *err_target = name_node ? name_node : head;
    if (error) {
      error->start = node_get_start_offset(err_target);
      error->end = node_get_end_offset(err_target);
      error->type = DOCUMENT_ERROR_TYPE_UNDEFINED_FUNCTION;
      error->message = g_strdup_printf("Undefined function %s", fn_name);
    }
    if (target)
      *target = err_target;
    return FALSE;
  }
  const Node *lambda = function_get_lambda_list(function);
  guint min_args = 0;
  guint max_args = 0;
  gboolean has_max = TRUE;
  if (!analyse_call_lambda_arity(lambda, &min_args, &max_args, &has_max))
    return TRUE;
  guint actual = expr->children->len > 0 ? expr->children->len - 1 : 0;
  if (actual < min_args || (has_max && actual > max_args)) {
    guint expected = actual < min_args ? min_args : max_args;
    if (error) {
      error->start = node_get_start_offset(expr);
      error->end = node_get_end_offset(expr);
      error->type = DOCUMENT_ERROR_TYPE_GENERIC;
      error->message = g_strdup_printf(
          "Expected %u arguments for %s but found %u", expected, fn_name,
          actual);
    }
    if (target)
      *target = expr;
    return FALSE;
  }
  return TRUE;
}

