#include "node.h"
#include <string.h>

static gboolean node_sdt_rule_contains_offset(const Node *node, gsize offset) {
  gsize start = node_get_start_offset(node);
  gsize end = node_get_end_offset(node);
  return offset >= start && offset <= end;
}

void node_set_sd_type(Node *node, StringDesignatorType sd_type, const gchar *package_context) {
  if (!node) return;
  node->sd_type = sd_type;
  g_free(node->package_context);
  node->package_context = package_context ? g_strdup(package_context) : NULL;
}


static void node_finalize(Node *node) {
  g_clear_pointer(&node->package_context, g_free);
  g_clear_pointer(&node->name, g_free);
}

Node *node_new(LispAstNodeType type, Document *document) {
  Node *node = g_new0(Node, 1);
  g_atomic_int_set(&node->ref, 1);
  node->type = type;
  node->document = document;
  return node;
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

void node_free_deep(Node *node) {
  if (!node)
    return;
  if (node->children) {
    for (guint i = 0; i < node->children->len; i++)
      node_free_deep(g_array_index(node->children, Node*, i));
    g_array_free(node->children, TRUE);
    node->children = NULL;
  }
  node_unref(node);
}

gboolean node_is(const Node *node, StringDesignatorType t) {
  return node && node->sd_type == t;
}

gboolean node_is_toplevel(const Node *node) {
  g_assert(node);
  g_assert(node->parent);
  return node->parent->parent == NULL;
}

gsize node_get_start_offset(const Node *node) {
  g_return_val_if_fail(node, 0);
  if (node->start_token)
    return node->start_token->start_offset;
  if (!node->children || node->children->len == 0)
    return 0;
  Node *child = g_array_index(node->children, Node*, 0);
  return node_get_start_offset(child);
}

gsize node_get_end_offset(const Node *node) {
  g_return_val_if_fail(node, 0);
  if (node->end_token)
    return node->end_token->end_offset;
  if (node->children && node->children->len > 0) {
    Node *child = g_array_index(node->children, Node*, node->children->len - 1);
    return node_get_end_offset(child);
  }
  if (node->start_token)
    return node->start_token->end_offset;
  // A root node can be empty when the document is empty.
  return 0;
}

const Node *node_find_containing_range(const Node *node, gsize start, gsize end) {
  g_return_val_if_fail(node, NULL);
  gsize node_start = node_get_start_offset(node);
  gsize node_end = node_get_end_offset(node);
  if (start < node_start || end > node_end)
    return NULL;
  if (node->children) {
    for (guint i = 0; i < node->children->len; i++) {
      const Node *child = g_array_index(node->children, Node*, i);
      const Node *found = node_find_containing_range(child, start, end);
      if (found)
        return found;
    }
  }
  return node;
}

const Node *node_find_sdt_containing_offset(const Node *node, gsize offset) {
  g_return_val_if_fail(node, NULL);
  if (!node_sdt_rule_contains_offset(node, offset))
    return NULL;
  if (node->children) {
    for (guint i = 0; i < node->children->len; i++) {
      const Node *child = g_array_index(node->children, Node*, i);
      if (!node_sdt_rule_contains_offset(child, offset))
        continue;
      const Node *found = node_find_sdt_containing_offset(child, offset);
      if (found)
        return found;
    }
  }
  if (node->sd_type != SDT_NONE)
    return node;
  return NULL;
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

gchar *node_debug_string(const Node *node) {
  if (!node || node->sd_type == SDT_NONE)
    return NULL;
  const gchar *type = node_sd_type_to_string(node->sd_type);
  return g_strdup(type);
}

static gchar *join_children_with_spaces(const Node *node) {
  if (!node->children || node->children->len == 0)
    return NULL;
  GString *str = g_string_new(NULL);
  for (guint i = 0; i < node->children->len; i++) {
    if (i > 0)
      g_string_append_c(str, ' ');
    Node *child = g_array_index(node->children, Node*, i);
    gchar *child_str = node_to_string(child);
    if (child_str) {
      g_string_append(str, child_str);
      g_free(child_str);
    }
  }
  return g_string_free(str, FALSE);
}

gchar *node_to_string(const Node *node) {
  if (!node)
    return NULL;
  if (!node->children || node->children->len == 0) {
    if (node->type == LISP_AST_NODE_TYPE_SYMBOL_NAME ||
        node->type == LISP_AST_NODE_TYPE_SYMBOL_PACKAGE) {
      return node->start_token ? g_ascii_strup(node->start_token->text, -1) : NULL;
    }
    return node->start_token ? g_strdup(node->start_token->text) : NULL;
  }
  if (node->type == LISP_AST_NODE_TYPE_LIST) {
    GString *str = g_string_new(node->start_token ? node->start_token->text : "(");
    gchar *children = join_children_with_spaces(node);
    if (children) {
      g_string_append(str, children);
      g_free(children);
    }
    g_string_append(str, node->end_token ? node->end_token->text : ")");
    return g_string_free(str, FALSE);
  }
  if (node->type == LISP_AST_NODE_TYPE_QUOTE ||
      node->type == LISP_AST_NODE_TYPE_BACKQUOTE ||
      node->type == LISP_AST_NODE_TYPE_UNQUOTE ||
      node->type == LISP_AST_NODE_TYPE_UNQUOTE_SPLICING) {
    GString *str = g_string_new(node->start_token ? node->start_token->text : "");
    if (node->children && node->children->len > 0) {
      gchar *child = node_to_string(g_array_index(node->children, Node*, 0));
      if (child) {
        g_string_append(str, child);
        g_free(child);
      }
    }
    return g_string_free(str, FALSE);
  }
  return join_children_with_spaces(node);
}

Node *node_get_symbol_name_node(Node *node) {
  if (!node)
    return NULL;
  if (node->type == LISP_AST_NODE_TYPE_SYMBOL_NAME)
    return node;
  if (node->type != LISP_AST_NODE_TYPE_SYMBOL || !node->children)
    return NULL;
  for (guint i = 0; i < node->children->len; i++) {
    Node *child = g_array_index(node->children, Node*, i);
    if (child->type == LISP_AST_NODE_TYPE_SYMBOL_NAME)
      return child;
  }
  return NULL;
}

const Node *node_get_symbol_name_node_const(const Node *node) {
  return (const Node*)node_get_symbol_name_node((Node*)node);
}

const gchar *node_get_name(const Node *node) {
  if (!node)
    return NULL;
  if (node->name)
    return node->name;
  if (node->type == LISP_AST_NODE_TYPE_SYMBOL_NAME) {
    g_assert(node->start_token && node->start_token->text);
    ((Node*)node)->name = g_ascii_strup(node->start_token->text, -1);
    return node->name;
  }
  g_assert(node->start_token && node->start_token->text);
  g_assert(node->type == LISP_AST_NODE_TYPE_STRING ||
          node->type == LISP_AST_NODE_TYPE_SYMBOL);
  const gchar *text;
  gchar *name;
  if (node->type == LISP_AST_NODE_TYPE_STRING) {
    text = node->start_token->text;
    g_assert(g_str_has_prefix(text, "\"") && g_str_has_suffix(text, "\""));
    name = g_strndup(text + 1, strlen(text) - 2);
  } else {
    const LispToken *tok = node->start_token;
    if (node->children) {
      for (guint i = 0; i < node->children->len; i++) {
        const Node *child = g_array_index(node->children, Node*, i);
        if (child->type == LISP_AST_NODE_TYPE_SYMBOL_NAME && child->start_token) {
          tok = child->start_token;
          break;
        }
      }
    }
    text = tok->text;
    const gchar *colon = g_strrstr(text, ":");
    const gchar *sym_name = colon ? colon + 1 : text;
    name = g_ascii_strup(sym_name, -1);
  }
  ((Node*)node)->name = name;
  return name;
}

