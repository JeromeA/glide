#include "node_info.h"
#include "package.h"

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
  node_info_ref(fn);
  return fn;
}

void function_info_unref(FunctionInfo *fn) {
  if (!fn) return;
  node_info_unref(fn);
}

NodeInfo *node_info_new(NodeInfoKind kind) {
  NodeInfo *ni = g_new0(NodeInfo, 1);
  ni->kind = kind;
  g_atomic_int_set(&ni->ref, 1);
  return ni;
}

NodeInfo *node_info_new_var_use(VariableInfo *var) {
  NodeInfo *ni = node_info_new(NODE_INFO_VAR_USE);
  ni->var = var ? variable_info_ref(var) : NULL;
  if (var && var->usages)
    g_ptr_array_add(var->usages, ni);
  return ni;
}

NodeInfo *node_info_new_var_def(VariableInfo *var_new) {
  NodeInfo *ni = node_info_new(NODE_INFO_VAR_DEF);
  ni->var = var_new ? variable_info_ref(var_new) : NULL;
  if (var_new)
    var_new->definition = ni;
  return ni;
}

NodeInfo *node_info_new_struct_field(const gchar *field_name) {
  NodeInfo *ni = node_info_new(NODE_INFO_STRUCT_FIELD);
  ni->field_name = field_name ? g_strdup(field_name) : NULL;
  ni->methods = g_ptr_array_new();
  return ni;
}

NodeInfo *node_info_new_package_def(Package *package) {
  NodeInfo *ni = node_info_new(NODE_INFO_PACKAGE_DEF);
  ni->package = package ? package_ref(package) : NULL;
  return ni;
}

void node_info_finalize(NodeInfo *ni) {
  switch (ni->kind) {
    case NODE_INFO_VAR_USE:
      if (ni->var) {
        if (ni->var->usages)
          g_ptr_array_remove(ni->var->usages, ni);
        variable_info_unref(ni->var);
      }
      break;
    case NODE_INFO_VAR_DEF:
      if (ni->var) {
        if (ni->var->definition == ni)
          ni->var->definition = NULL;
        variable_info_unref(ni->var);
      }
      break;
    case NODE_INFO_PACKAGE_DEF:
      if (ni->package)
        package_unref(ni->package);
      break;
    case NODE_INFO_STRUCT_FIELD:
      g_clear_pointer(&ni->field_name, g_free);
      if (ni->methods) {
        for (guint i = 0; i < ni->methods->len; i++) {
          FunctionInfo *fn = g_ptr_array_index(ni->methods, i);
          function_info_unref(fn);
        }
        g_ptr_array_free(ni->methods, TRUE);
      }
      break;
    default:
      break;
  }
}

const gchar *node_info_kind_to_string(NodeInfoKind kind) {
  switch(kind) {
    case NODE_INFO_VAR_DEF: return "VarDef";
    case NODE_INFO_VAR_USE: return "VarUse";
    case NODE_INFO_FUNCTION_DEF: return "FunctionDef";
    case NODE_INFO_FUNCTION_USE: return "FunctionUse";
    case NODE_INFO_PACKAGE_DEF: return "PackageDef";
    case NODE_INFO_STRUCT_FIELD: return "StructField";
    default: return "None";
  }
}

gchar *node_info_to_string(const NodeInfo *ni) {
  if (!ni)
    return NULL;
  const gchar *type = node_info_kind_to_string(ni->kind);
  switch(ni->kind) {
    case NODE_INFO_STRUCT_FIELD:
      if (ni->field_name)
        return g_strdup_printf("%s %s", type, ni->field_name);
      return g_strdup(type);
    default:
      return g_strdup(type);
  }
}

