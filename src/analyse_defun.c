#include "analyse_defun.h"
#include "analyse.h"
#include "function.h"
#include "project_file.h"

static void analyse_defun_mark_error(Node *node, const gchar *message) {
  if (!node || !node->file)
    return;
  gsize start = node_get_start_offset(node);
  gsize end = node_get_end_offset(node);
  if (end <= start)
    return;
  project_file_add_error(node->file, start, end, message);
}

void analyse_defun(Project *project, Node *expr, AnalyseContext *context) {
  g_return_if_fail(expr);
  g_return_if_fail(expr->children);

  g_return_if_fail(expr->children->len > 1);
  Node *name_node = g_array_index(expr->children, Node*, 1);
  g_return_if_fail(name_node);
  if (name_node->type != LISP_AST_NODE_TYPE_SYMBOL) {
    analyse_defun_mark_error(name_node, "DEFUN requires a symbol name.");
    return;
  }
  Node *name_symbol = node_get_symbol_name_node(name_node);
  g_return_if_fail(name_symbol);
  if (!name_symbol->sd_type)
    node_set_sd_type(name_symbol, SDT_FUNCTION_DEF, context->package);

  g_return_if_fail(expr->children->len > 2);
  Node *args = g_array_index(expr->children, Node*, 2);
  g_return_if_fail(args);
  if (args->type != LISP_AST_NODE_TYPE_LIST) {
    analyse_defun_mark_error(args, "DEFUN requires a parameter list.");
    return;
  }
  if (args->type == LISP_AST_NODE_TYPE_LIST && args->children) {
    for (guint i = 0; i < args->children->len; i++) {
      Node *arg = g_array_index(args->children, Node*, i);
      if (arg->type == LISP_AST_NODE_TYPE_SYMBOL) {
        Node *arg_name = node_get_symbol_name_node(arg);
        if (arg_name && !arg_name->sd_type)
          node_set_sd_type(arg_name, SDT_VAR_DEF, context->package);
      }
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
      FUNCTION_KIND_FUNCTION, node_get_name(name_node), context->package, NULL);
  if (node_is_toplevel(expr))
    project_add_function(project, function);
  function_unref(function);

  guint start = doc_node ? 4 : 3;
  for (guint i = start; i < expr->children->len; i++) {
    Node *child = g_array_index(expr->children, Node*, i);
    analyse_node(project, child, context);
  }
}

