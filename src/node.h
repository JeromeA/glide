#pragma once

#include <glib.h>
#include "lisp_lexer.h"

typedef enum {
  LISP_AST_NODE_TYPE_NUMBER,
  LISP_AST_NODE_TYPE_SYMBOL,
  LISP_AST_NODE_TYPE_SYMBOL_PACKAGE,
  LISP_AST_NODE_TYPE_SYMBOL_SEPARATOR,
  LISP_AST_NODE_TYPE_SYMBOL_NAME,
  LISP_AST_NODE_TYPE_LIST,
  LISP_AST_NODE_TYPE_STRING,
  LISP_AST_NODE_TYPE_QUOTE,
  LISP_AST_NODE_TYPE_BACKQUOTE,
  LISP_AST_NODE_TYPE_UNQUOTE,
  LISP_AST_NODE_TYPE_UNQUOTE_SPLICING,
} LispAstNodeType;

typedef enum {
  SDT_NONE = 0,
  SDT_VAR_DEF,
  SDT_VAR_USE,
  SDT_FUNCTION_DEF,
  SDT_FUNCTION_USE,
  SDT_PACKAGE_DEF,
  SDT_PACKAGE_USE,
  SDT_STRUCT_FIELD,
} StringDesignatorType;

typedef struct Node Node;
typedef struct _ProjectFile ProjectFile;

struct Node {
  LispAstNodeType type;
  const LispToken *start_token;
  const LispToken *end_token;
  Node *parent;
  GArray *children; /* Node* */
  ProjectFile *file;

  StringDesignatorType sd_type;
  gint ref;
  gchar *package_context;
  gchar *name;
};
void node_set_sd_type(Node *node, StringDesignatorType sd_type, const gchar *package_context);

Node *node_new(LispAstNodeType type, ProjectFile *file);
Node *node_ref(Node *node);
void node_unref(Node *node);
gboolean node_is(const Node *node, StringDesignatorType t);
gboolean node_is_toplevel(const Node *node);
gsize node_get_start_offset(const Node *node);
gsize node_get_end_offset(const Node *node);
const Node *node_find_containing_range(const Node *node, gsize start, gsize end);
const gchar *node_sd_type_to_string(StringDesignatorType sd_type);
gchar *node_debug_string(const Node *node);
gchar *node_to_string(const Node *node);
const gchar *node_get_name(const Node *node);
Node       *node_get_symbol_name_node(Node *node);
const Node *node_get_symbol_name_node_const(const Node *node);

