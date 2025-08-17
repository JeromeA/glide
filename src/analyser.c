#include "analyser.h"
#include "node_info.h"

static void analyse_node(LispAstNode *node) {
  if (!node)
    return;

  if (node->type == LISP_AST_NODE_TYPE_LIST && node->children) {
    if (node->children->len > 0) {
      LispAstNode *first = g_array_index(node->children, LispAstNode*, 0);
      if (first->type == LISP_AST_NODE_TYPE_SYMBOL) {
        if (!first->node_info)
          first->node_info = node_info_new(NODE_INFO_FUNCTION_USE);
        if (first->start_token && first->start_token->text &&
            g_ascii_strcasecmp(first->start_token->text, "defun") == 0 &&
            node->children->len > 1) {
          LispAstNode *name = g_array_index(node->children, LispAstNode*, 1);
          if (name->type == LISP_AST_NODE_TYPE_SYMBOL && !name->node_info)
            name->node_info = node_info_new(NODE_INFO_FUNCTION_DEF);
        }
      }
    }
    for (guint i = 0; i < node->children->len; i++)
      analyse_node(g_array_index(node->children, LispAstNode*, i));
  }
}

void analyse_ast(LispAstNode *root) {
  analyse_node(root);
}

