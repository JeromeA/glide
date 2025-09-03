#include "analyse_defpackage.h"
#include "package.h"
#include <string.h>

static void parse_symbols(Node *list, guint start,
    void (*add)(Package *package, const gchar *symbol), Package *package,
    const gchar *context, StringDesignatorType sd_type) {
  if (!list || !list->children) return;
  for (guint i = start; i < list->children->len; i++) {
    Node *sym = g_array_index(list->children, Node*, i);
    const gchar *name = node_get_name(sym);
    if (name) {
      if (add)
        add(package, name);
      if (sd_type != SDT_NONE)
        node_set_sd_type(sym, sd_type, context);
    }
  }
}

void analyse_defpackage(Project *project, Node *expr, const gchar *context) {
  if (!project || !expr || !expr->children || expr->children->len < 2) return;

  Node *name_node = g_array_index(expr->children, Node*, 1);
  const gchar *pkg_name = node_get_name(name_node);
  if (!pkg_name) return;

  Package *pkg = package_new(pkg_name);
  node_set_sd_type(name_node, SDT_PACKAGE_DEF, context);

  for (guint i = 2; i < expr->children->len; i++) {
    Node *option = g_array_index(expr->children, Node*, i);
    if (!option || option->type != LISP_AST_NODE_TYPE_LIST || !option->children || option->children->len == 0)
      continue;
    Node *keyword_node = g_array_index(option->children, Node*, 0);
    const gchar *keyword = node_get_name(keyword_node);
    if (!keyword) continue;

    if (strcmp(keyword, "NICKNAMES") == 0) {
      parse_symbols(option, 1, (void(*)(Package*, const gchar*))package_add_nickname, pkg, context, SDT_PACKAGE_DEF);
    } else if (strcmp(keyword, "USE") == 0) {
      parse_symbols(option, 1, (void(*)(Package*, const gchar*))package_add_use, pkg, context, SDT_PACKAGE_USE);
    } else if (strcmp(keyword, "EXPORT") == 0) {
      parse_symbols(option, 1, (void(*)(Package*, const gchar*))package_add_export, pkg, context, SDT_NONE);
    } else if (strcmp(keyword, "SHADOW") == 0) {
      parse_symbols(option, 1, (void(*)(Package*, const gchar*))package_add_shadow, pkg, context, SDT_NONE);
    } else if (strcmp(keyword, "IMPORT-FROM") == 0) {
      if (option->children->len > 2) {
        Node *from_node = g_array_index(option->children, Node*, 1);
        const gchar *from_name = node_get_name(from_node);
        if (from_name) {
          node_set_sd_type(from_node, SDT_PACKAGE_USE, context);
          for (guint j = 2; j < option->children->len; j++) {
            Node *sym = g_array_index(option->children, Node*, j);
            const gchar *sym_name = node_get_name(sym);
            if (sym_name) {
              package_add_import_from(pkg, sym_name, from_name);
            }
          }
        }
      }
    } else if (strcmp(keyword, "DOCUMENTATION") == 0) {
      if (option->children->len > 1) {
        Node *str = g_array_index(option->children, Node*, 1);
        const gchar *desc = node_get_name(str);
        if (desc)
          package_set_description(pkg, desc);
      }
    }
  }

  if (node_is_toplevel(expr))
    project_add_package(project, pkg);
  package_unref(pkg);
}

