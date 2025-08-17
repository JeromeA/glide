#include "node.h"
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
  return node_ref(fn);
}

void function_info_unref(FunctionInfo *fn) {
  if (!fn) return;
  node_unref(fn);
}

void node_set_sd_type(Node *node, StringDesignatorType sd_type) {
  if (!node) return;
  node->sd_type = sd_type;
}

void node_set_var_use(Node *node, VariableInfo *var) {
  if (!node) return;
  node->sd_type = SDT_VAR_USE;
  node->var = var ? variable_info_ref(var) : NULL;
  if (var && var->usages)
    g_ptr_array_add(var->usages, node);
}

void node_set_var_def(Node *node, VariableInfo *var_new) {
  if (!node) return;
  node->sd_type = SDT_VAR_DEF;
  node->var = var_new ? variable_info_ref(var_new) : NULL;
  if (var_new)
    var_new->definition = node;
}

void node_set_struct_field(Node *node, const gchar *field_name) {
  if (!node) return;
  node->sd_type = SDT_STRUCT_FIELD;
  node->field_name = field_name ? g_strdup(field_name) : NULL;
  node->methods = g_ptr_array_new();
}

void node_set_package_def(Node *node, Package *package) {
  if (!node) return;
  node->sd_type = SDT_PACKAGE_DEF;
  node->package = package ? package_ref(package) : NULL;
}

void node_set_package_use(Node *node, Package *package) {
  if (!node) return;
  node->sd_type = SDT_PACKAGE_USE;
  node->package = package ? package_ref(package) : NULL;
}

static void node_finalize(Node *node) {
  switch (node->sd_type) {
    case SDT_VAR_USE:
      if (node->var) {
        if (node->var->usages)
          g_ptr_array_remove(node->var->usages, node);
        variable_info_unref(node->var);
      }
      break;
    case SDT_VAR_DEF:
      if (node->var) {
        if (node->var->definition == node)
          node->var->definition = NULL;
        variable_info_unref(node->var);
      }
      break;
    case SDT_PACKAGE_DEF:
    case SDT_PACKAGE_USE:
      if (node->package)
        package_unref(node->package);
      break;
    case SDT_STRUCT_FIELD:
      g_clear_pointer(&node->field_name, g_free);
      if (node->methods) {
        for (guint i = 0; i < node->methods->len; i++) {
          FunctionInfo *fn = g_ptr_array_index(node->methods, i);
          function_info_unref(fn);
        }
        g_ptr_array_free(node->methods, TRUE);
        node->methods = NULL;
      }
      break;
    default:
      break;
  }
}

Node *node_ref(Node *node) {
  if (!node) return NULL;
  g_atomic_int_inc(&node->ref);
  return node;
}

void node_unref(Node *node) {
  if (!node) return;
  if (g_atomic_int_dec_and_test(&node->ref)) {
    node_finalize(node);
    g_free(node);
  }
}

gboolean node_is(const Node *node, StringDesignatorType t) {
  return node && node->sd_type == t;
}

const gchar *node_sd_type_to_string(StringDesignatorType sd_type) {
  switch(sd_type) {
    case SDT_VAR_DEF: return "VarDef";
    case SDT_VAR_USE: return "VarUse";
    case SDT_FUNCTION_DEF: return "FunctionDef";
    case SDT_FUNCTION_USE: return "FunctionUse";
    case SDT_PACKAGE_DEF: return "PackageDef";
    case SDT_PACKAGE_USE: return "PackageUse";
    case SDT_STRUCT_FIELD: return "StructField";
    default: return "None";
  }
}

gchar *node_to_string(const Node *node) {
  if (!node || node->sd_type == SDT_NONE)
    return NULL;
  const gchar *type = node_sd_type_to_string(node->sd_type);
  switch(node->sd_type) {
    case SDT_STRUCT_FIELD:
      if (node->field_name)
        return g_strdup_printf("%s %s", type, node->field_name);
      return g_strdup(type);
    default:
      return g_strdup(type);
  }
}

const gchar *node_get_name(const Node *node) {
  if (!node)
    return NULL;
  switch(node->sd_type) {
    case SDT_STRUCT_FIELD:
      return node->field_name;
    case SDT_PACKAGE_DEF:
    case SDT_PACKAGE_USE:
      return node->package ? node->package->name : NULL;
    default:
      return node->start_token ? node->start_token->text : NULL;
  }
}

