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

typedef struct VariableInfo VariableInfo;
typedef struct Node Node;
typedef Node FunctionInfo;
typedef struct Package Package;

struct VariableInfo {
  gint ref;
  Node *definition; /* SDT_VAR_DEF */
  GPtrArray *usages; /* Node* (SDT_VAR_USE) */
};

struct Node {
  LispAstNodeType type;
  const LispToken *start_token;
  const LispToken *end_token;
  GArray *children; /* Node* */

  StringDesignatorType sd_type;
  gint ref;
  VariableInfo *var;
  Package *package;
  gchar *package_context;
  gchar *name;
  gchar *field_name;
  GPtrArray *methods; /* FunctionInfo* */
};

VariableInfo *variable_info_new(void);
VariableInfo *variable_info_ref(VariableInfo *var);
void variable_info_unref(VariableInfo *var);
FunctionInfo *function_info_ref(FunctionInfo *fn);
void function_info_unref(FunctionInfo *fn);

void node_set_sd_type(Node *node, StringDesignatorType sd_type, const gchar *package_context);
void node_set_var_use(Node *node, VariableInfo *var, const gchar *package_context);
void node_set_var_def(Node *node, VariableInfo *var_new, const gchar *package_context);
void node_set_struct_field(Node *node, const gchar *field_name, const gchar *package_context);
void node_set_package_def(Node *node, Package *package, const gchar *package_context);
void node_set_package_use(Node *node, Package *package, const gchar *package_context);

Node *node_ref(Node *node);
void node_unref(Node *node);
gboolean node_is(const Node *node, StringDesignatorType t);
const gchar *node_sd_type_to_string(StringDesignatorType sd_type);
gchar *node_to_string(const Node *node);
const gchar *node_get_name(const Node *node);

