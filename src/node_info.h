#ifndef NODE_INFO_H
#define NODE_INFO_H

#include <glib.h>

typedef enum {
  NODE_INFO_NONE = 0,
  NODE_INFO_VAR_DEF,
  NODE_INFO_VAR_USE,
  NODE_INFO_FUNCTION_DEF,
  NODE_INFO_FUNCTION_USE,
  NODE_INFO_STRUCT_FIELD,
} NodeInfoKind;

typedef struct VariableInfo VariableInfo;
typedef struct NodeInfo NodeInfo;
typedef NodeInfo FunctionInfo;

struct NodeInfo {
  NodeInfoKind kind;
  gint ref;
  VariableInfo *var;
  gchar *field_name;
  GPtrArray *methods; /* FunctionInfo* */
};

struct VariableInfo {
  gint ref;
  NodeInfo *definition; /* NODE_INFO_VAR_DEF */
  GPtrArray *usages;    /* NodeInfo* (NODE_INFO_VAR_USE) */
};

VariableInfo *variable_info_new(void);
VariableInfo *variable_info_ref(VariableInfo *var);
void variable_info_unref(VariableInfo *var);
FunctionInfo *function_info_ref(FunctionInfo *fn);
void function_info_unref(FunctionInfo *fn);

NodeInfo *node_info_new(NodeInfoKind kind);
NodeInfo *node_info_new_var_use(VariableInfo *var);
NodeInfo *node_info_new_var_def(VariableInfo *var_new);
NodeInfo *node_info_new_struct_field(const gchar *field_name);

static inline NodeInfo *node_info_ref(NodeInfo *ni) {
  g_atomic_int_inc(&ni->ref);
  return ni;
}

void node_info_finalize(NodeInfo *ni);

static inline void node_info_unref(NodeInfo *ni) {
  if (g_atomic_int_dec_and_test(&ni->ref)) {
    node_info_finalize(ni);
    g_free(ni);
  }
}

static inline gboolean node_info_is(const NodeInfo *ni, NodeInfoKind k) {
  return ni && ni->kind == k;
}

const gchar *node_info_kind_to_string(NodeInfoKind kind);
gchar *node_info_to_string(const NodeInfo *ni);

#endif // NODE_INFO_H

