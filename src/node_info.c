#include "node_info.h"

struct FunctionInfo {
  NodeInfo base;
};

VariableInfo *variable_info_new(void) {
  VariableInfo *var = g_new0(VariableInfo, 1);
  g_atomic_int_set(&var->ref, 1);
  var->usages = g_ptr_array_new();
  return var;
}

VariableInfo *variable_info_ref(VariableInfo *var) {
  if (!var) return NULL;
  g_atomic_int_inc(&var->ref);
  return var;
}

void variable_info_unref(VariableInfo *var) {
  if (!var) return;
  if (g_atomic_int_dec_and_test(&var->ref)) {
    if (var->usages)
      g_ptr_array_free(var->usages, TRUE);
    g_free(var);
  }
}

FunctionInfo *function_info_ref(FunctionInfo *fn) {
  if (!fn) return NULL;
  node_info_ref(&fn->base);
  return fn;
}

void function_info_unref(FunctionInfo *fn) {
  if (!fn) return;
  node_info_unref(&fn->base);
}

static void var_use_info_finalize(NodeInfo *ni) {
  VarUseInfo *s = VAR_USE_INFO(ni);
  if (s->var) {
    if (s->var->usages)
      g_ptr_array_remove(s->var->usages, s);
    variable_info_unref(s->var);
  }
}

static void var_def_info_finalize(NodeInfo *ni) {
  VarDefInfo *s = VAR_DEF_INFO(ni);
  if (s->var) {
    if (s->var->definition == s)
      s->var->definition = NULL;
    variable_info_unref(s->var);
  }
}

static void struct_field_info_finalize(NodeInfo *ni) {
  StructFieldInfo *s = STRUCT_FIELD_INFO(ni);
  g_clear_pointer(&s->field_name, g_free);
  if (s->methods) {
    for (guint i = 0; i < s->methods->len; i++) {
      FunctionInfo *fn = g_ptr_array_index(s->methods, i);
      function_info_unref(fn);
    }
    g_ptr_array_free(s->methods, TRUE);
  }
}

VarUseInfo *var_use_info_new(VariableInfo *var) {
  VarUseInfo *s = g_new0(VarUseInfo, 1);
  node_info_init(&s->base, NODE_INFO_VAR_USE, var_use_info_finalize);
  s->var = var ? variable_info_ref(var) : NULL;
  if (var && var->usages)
    g_ptr_array_add(var->usages, s);
  return s;
}

VarDefInfo *var_def_info_new(VariableInfo *var_new) {
  VarDefInfo *s = g_new0(VarDefInfo, 1);
  node_info_init(&s->base, NODE_INFO_VAR_DEF, var_def_info_finalize);
  s->var = var_new ? variable_info_ref(var_new) : NULL;
  if (var_new)
    var_new->definition = s;
  return s;
}

StructFieldInfo *struct_field_info_new(const gchar *field_name) {
  StructFieldInfo *s = g_new0(StructFieldInfo, 1);
  node_info_init(&s->base, NODE_INFO_STRUCT_FIELD, struct_field_info_finalize);
  s->field_name = field_name ? g_strdup(field_name) : NULL;
  s->methods = g_ptr_array_new();
  return s;
}

const gchar *node_info_kind_to_string(NodeInfoKind kind) {
  switch(kind) {
    case NODE_INFO_VAR_DEF: return "VarDef";
    case NODE_INFO_VAR_USE: return "VarUse";
    case NODE_INFO_FUNCTION_DEF: return "FunctionDef";
    case NODE_INFO_FUNCTION_USE: return "FunctionUse";
    case NODE_INFO_STRUCT_FIELD: return "StructField";
    default: return "None";
  }
}

gchar *node_info_to_string(const NodeInfo *ni) {
  if (!ni)
    return NULL;
  const gchar *type = node_info_kind_to_string(ni->kind);
  switch(ni->kind) {
    case NODE_INFO_STRUCT_FIELD: {
      const StructFieldInfo *s = STRUCT_FIELD_INFO(ni);
      if (s->field_name)
        return g_strdup_printf("%s %s", type, s->field_name);
      return g_strdup(type);
    }
    default:
      return g_strdup(type);
  }
}
