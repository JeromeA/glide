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

typedef struct NodeInfo {
  NodeInfoKind kind;
  gint         ref;
  void       (*finalize)(struct NodeInfo *self);
} NodeInfo;

static inline void node_info_init(NodeInfo *ni, NodeInfoKind kind,
                                  void (*finalize)(NodeInfo*)) {
  ni->kind = kind;
  g_atomic_int_set(&ni->ref, 1);
  ni->finalize = finalize;
}

static inline NodeInfo *node_info_ref(NodeInfo *ni) {
  g_atomic_int_inc(&ni->ref);
  return ni;
}

static inline void node_info_unref(NodeInfo *ni) {
  if (g_atomic_int_dec_and_test(&ni->ref)) {
    if (ni->finalize) ni->finalize(ni);
    g_free(ni);
  }
}

typedef struct VariableInfo VariableInfo;
typedef struct FunctionInfo  FunctionInfo;
typedef struct StructFieldInfo StructFieldInfo;

VariableInfo *variable_info_ref(VariableInfo *var);
void variable_info_unref(VariableInfo *var);
void function_info_unref(FunctionInfo *fn);

typedef struct {
  NodeInfo base;
  VariableInfo *var;
} VarUseInfo;

typedef struct {
  NodeInfo base;
  VariableInfo *var;
} VarDefInfo;

struct StructFieldInfo {
  NodeInfo base;
  gchar *field_name;
  GPtrArray *methods;
};

static inline gboolean node_info_is(const NodeInfo *ni, NodeInfoKind k) {
  return ni && ni->kind == k;
}
#define VAR_USE_INFO(ni)    ((VarUseInfo*)(ni))
#define VAR_DEF_INFO(ni)    ((VarDefInfo*)(ni))
#define STRUCT_FIELD_INFO(ni) ((StructFieldInfo*)(ni))

VarUseInfo *var_use_info_new(VariableInfo *var);
VarDefInfo *var_def_info_new(VariableInfo *var_new);
StructFieldInfo *struct_field_info_new(const gchar *field_name);

const gchar *node_info_kind_to_string(NodeInfoKind kind);
gchar *node_info_to_string(const NodeInfo *ni);

#endif // NODE_INFO_H
