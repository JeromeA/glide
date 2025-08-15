#include "node_info.h"

static void var_use_info_finalize(NodeInfo *ni) {
  VarUseInfo *s = VAR_USE_INFO(ni);
  if (s->var) variable_info_unref(s->var);
}

static void var_def_info_finalize(NodeInfo *ni) {
  VarDefInfo *s = VAR_DEF_INFO(ni);
  if (s->var) variable_info_unref(s->var);
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
  return s;
}

VarDefInfo *var_def_info_new(VariableInfo *var_new) {
  VarDefInfo *s = g_new0(VarDefInfo, 1);
  node_info_init(&s->base, NODE_INFO_VAR_DEF, var_def_info_finalize);
  s->var = var_new ? variable_info_ref(var_new) : NULL;
  return s;
}

StructFieldInfo *struct_field_info_new(const gchar *field_name) {
  StructFieldInfo *s = g_new0(StructFieldInfo, 1);
  node_info_init(&s->base, NODE_INFO_STRUCT_FIELD, struct_field_info_finalize);
  s->field_name = field_name ? g_strdup(field_name) : NULL;
  s->methods = g_ptr_array_new();
  return s;
}
