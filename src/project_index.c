#include "project_index.h"
#include "node.h"
#include "package.h"
#include "function.h"
#include "document.h"
#include "util.h"

struct _ProjectIndex {
  GHashTable *function_defs; /* name -> GPtrArray* Node* */
  GHashTable *function_uses;
  GHashTable *variable_defs;
  GHashTable *variable_uses;
  GHashTable *package_defs;
  GHashTable *package_uses;
  GHashTable *packages; /* name -> Package* */
  GHashTable *functions; /* name -> Function* */
  GHashTable *functions_by_package; /* package -> name->Function* */
  GHashTable *variables; /* name -> documentation */
  GHashTable *variables_by_package; /* package -> name->doc */
};

static GHashTable *project_index_table(ProjectIndex *self, StringDesignatorType sd_type);
static void project_index_add_to(GHashTable *table, const gchar *name, Node *node);
static void project_index_node(ProjectIndex *self, const Node *node);
static void project_index_remove_from_table(GHashTable *table, Document *document);

ProjectIndex *project_index_new(void) {
  ProjectIndex *self = g_new0(ProjectIndex, 1);
  self->function_defs = g_hash_table_new_full(g_str_hash, g_str_equal, g_free, (GDestroyNotify)g_ptr_array_unref);
  self->function_uses = g_hash_table_new_full(g_str_hash, g_str_equal, g_free, (GDestroyNotify)g_ptr_array_unref);
  self->variable_defs = g_hash_table_new_full(g_str_hash, g_str_equal, g_free, (GDestroyNotify)g_ptr_array_unref);
  self->variable_uses = g_hash_table_new_full(g_str_hash, g_str_equal, g_free, (GDestroyNotify)g_ptr_array_unref);
  self->package_defs = g_hash_table_new_full(g_str_hash, g_str_equal, g_free, (GDestroyNotify)g_ptr_array_unref);
  self->package_uses = g_hash_table_new_full(g_str_hash, g_str_equal, g_free, (GDestroyNotify)g_ptr_array_unref);
  self->packages = g_hash_table_new_full(g_str_hash, g_str_equal, g_free, (GDestroyNotify)package_unref);
  self->functions = g_hash_table_new_full(g_str_hash, g_str_equal, g_free, (GDestroyNotify)function_unref);
  self->functions_by_package = g_hash_table_new_full(g_str_hash, g_str_equal,
      g_free, (GDestroyNotify)g_hash_table_unref);
  self->variables = g_hash_table_new_full(g_str_hash, g_str_equal, g_free, g_free);
  self->variables_by_package = g_hash_table_new_full(g_str_hash, g_str_equal,
      g_free, (GDestroyNotify)g_hash_table_unref);
  return self;
}

void project_index_free(ProjectIndex *self) {
  if (!self) return;
  g_return_if_fail(glide_is_ui_thread());
  project_index_clear(self);
  g_clear_pointer(&self->function_defs, g_hash_table_unref);
  g_clear_pointer(&self->function_uses, g_hash_table_unref);
  g_clear_pointer(&self->variable_defs, g_hash_table_unref);
  g_clear_pointer(&self->variable_uses, g_hash_table_unref);
  g_clear_pointer(&self->package_defs, g_hash_table_unref);
  g_clear_pointer(&self->package_uses, g_hash_table_unref);
  g_clear_pointer(&self->packages, g_hash_table_unref);
  g_clear_pointer(&self->functions, g_hash_table_unref);
  g_clear_pointer(&self->functions_by_package, g_hash_table_unref);
  g_clear_pointer(&self->variables, g_hash_table_unref);
  g_clear_pointer(&self->variables_by_package, g_hash_table_unref);
  g_free(self);
}

static GHashTable *project_index_table(ProjectIndex *self, StringDesignatorType sd_type) {
  switch (sd_type) {
    case SDT_FUNCTION_DEF: return self->function_defs;
    case SDT_FUNCTION_USE: return self->function_uses;
    case SDT_VAR_DEF: return self->variable_defs;
    case SDT_VAR_USE: return self->variable_uses;
    case SDT_PACKAGE_DEF: return self->package_defs;
    case SDT_PACKAGE_USE: return self->package_uses;
    default: return NULL;
  }
}

static void project_index_add_to(GHashTable *table, const gchar *name, Node *node) {
  if (!table || !name || !node) return;
  GPtrArray *arr = g_hash_table_lookup(table, name);
  if (!arr) {
    arr = g_ptr_array_new_with_free_func((GDestroyNotify)node_unref);
    g_hash_table_insert(table, g_strdup(name), arr);
  }
  g_ptr_array_add(arr, node_ref(node));
  LOG(1, "Index: added %s as %s", name, node_sd_type_to_string(node->sd_type));
}

static void project_index_node(ProjectIndex *self, const Node *node) {
  if (!node || !node->sd_type) return;
  const gchar *name = node_get_name(node);
  GHashTable *table = project_index_table(self, node->sd_type);
  project_index_add_to(table, name, (Node*)node);
}

void project_index_walk(ProjectIndex *self, const Node *node) {
  g_return_if_fail(glide_is_ui_thread());
  if (!node) return;
  project_index_node(self, node);
  if (node->children)
    for (guint i = 0; i < node->children->len; i++)
      project_index_walk(self, g_array_index(node->children, Node*, i));
}

GHashTable *project_index_get(ProjectIndex *self, StringDesignatorType sd_type) {
  return project_index_table(self, sd_type);
}

void project_index_clear(ProjectIndex *self) {
  g_return_if_fail(glide_is_ui_thread());
  GHashTable *tables[] = { self->function_defs, self->function_uses,
    self->variable_defs, self->variable_uses, self->package_defs,
    self->package_uses };
  const gchar *names[] = { "function defs", "function uses",
    "variable defs", "variable uses", "package defs", "package uses" };
  for (guint t = 0; t < G_N_ELEMENTS(tables); t++)
    if (tables[t]) {
      g_hash_table_remove_all(tables[t]);
      LOG(1, "Index: cleared %s", names[t]);
    }
  if (self->packages) {
    g_hash_table_remove_all(self->packages);
    LOG(1, "Index: cleared packages");
  }
  if (self->functions) {
    g_hash_table_remove_all(self->functions);
    LOG(1, "Index: cleared functions");
  }
  if (self->functions_by_package) {
    g_hash_table_remove_all(self->functions_by_package);
    LOG(1, "Index: cleared functions by package");
  }
  if (self->variables) {
    g_hash_table_remove_all(self->variables);
    LOG(1, "Index: cleared variables");
  }
  if (self->variables_by_package) {
    g_hash_table_remove_all(self->variables_by_package);
    LOG(1, "Index: cleared variables by package");
  }
}

static void project_index_remove_from_table(GHashTable *table, Document *document) {
  if (!table) return;
  GHashTableIter iter;
  g_hash_table_iter_init(&iter, table);
  gpointer key, value;
  while (g_hash_table_iter_next(&iter, &key, &value)) {
    GPtrArray *arr = value;
    for (guint i = 0; i < arr->len; ) {
      Node *n = g_ptr_array_index(arr, i);
      if (n->document == document) {
        LOG(1, "Index: removed %s as %s", (const gchar*)key,
            node_sd_type_to_string(n->sd_type));
        g_ptr_array_remove_index(arr, i);
      } else
        i++;
    }
    if (arr->len == 0) {
      LOG(1, "Index: removed entry %s", (const gchar*)key);
      g_hash_table_iter_remove(&iter);
    }
  }
}

void project_index_remove_document(ProjectIndex *self, Document *document) {
  g_return_if_fail(glide_is_ui_thread());
  GHashTable *tables[] = { self->function_defs, self->function_uses,
    self->variable_defs, self->variable_uses, self->package_defs,
    self->package_uses };
  for (guint t = 0; t < G_N_ELEMENTS(tables); t++)
    project_index_remove_from_table(tables[t], document);

  if (self->functions) {
    GHashTableIter iter;
    g_hash_table_iter_init(&iter, self->functions);
    gpointer key, value;
    while (g_hash_table_iter_next(&iter, &key, &value)) {
      Function *fn = value;
      const Node *sym = function_get_symbol(fn);
      if (sym && sym->document == document) {
        LOG(1, "Index: removed function %s", (const gchar*)key);
        g_hash_table_iter_remove(&iter);
      }
    }
  }

  if (self->functions_by_package) {
    GHashTableIter piter;
    g_hash_table_iter_init(&piter, self->functions_by_package);
    gpointer pkey, pvalue;
    while (g_hash_table_iter_next(&piter, &pkey, &pvalue)) {
      GHashTable *tbl = pvalue;
      GHashTableIter iter;
      g_hash_table_iter_init(&iter, tbl);
      gpointer nkey, nvalue;
      while (g_hash_table_iter_next(&iter, &nkey, &nvalue)) {
        Function *fn = nvalue;
        const Node *sym = function_get_symbol(fn);
        if (sym && sym->document == document) {
          LOG(1, "Index: removed function %s from package %s",
              (const gchar*)nkey, (const gchar*)pkey);
          g_hash_table_iter_remove(&iter);
        }
      }
      if (g_hash_table_size(tbl) == 0) {
        LOG(1, "Index: removed package %s from functions index",
            (const gchar*)pkey);
        g_hash_table_iter_remove(&piter);
      }
    }
  }

  if (self->packages && self->package_defs) {
    GHashTableIter iter;
    g_hash_table_iter_init(&iter, self->package_defs);
    gpointer key, value;
    while (g_hash_table_iter_next(&iter, &key, &value)) {
      GPtrArray *arr = value;
      for (guint i = 0; i < arr->len; i++) {
        Node *n = g_ptr_array_index(arr, i);
        if (n->document == document) {
          LOG(1, "Index: removed package %s", (const gchar*)key);
          g_hash_table_remove(self->packages, key);
          break;
        }
      }
    }
  }
}

void project_index_add_package(ProjectIndex *self, Package *package) {
  g_return_if_fail(self != NULL);
  g_return_if_fail(package != NULL);
  g_return_if_fail(glide_is_ui_thread());
  const gchar *name = package_get_name(package);
  if (!name) return;
  g_hash_table_replace(self->packages, g_strdup(name), package_ref(package));
  LOG(1, "Index: added package %s", name);
}

Package *project_index_get_package(ProjectIndex *self, const gchar *name) {
  g_return_val_if_fail(self != NULL, NULL);
  g_return_val_if_fail(name != NULL, NULL);
  return g_hash_table_lookup(self->packages, name);
}

gchar **project_index_get_package_names(ProjectIndex *self, guint *length) {
  g_return_val_if_fail(self != NULL, NULL);
  return (gchar**)g_hash_table_get_keys_as_array(self->packages, length);
}

void project_index_add_function(ProjectIndex *self, Function *function) {
  g_return_if_fail(self != NULL);
  g_return_if_fail(function != NULL);
  g_return_if_fail(glide_is_ui_thread());
  const gchar *name = function_get_name(function);
  if (!name) return;
  g_hash_table_replace(self->functions, g_strdup(name), function_ref(function));
  LOG(1, "Index: added function %s", name);
  const gchar *pkg = function_get_package(function);
  if (pkg) {
    GHashTable *tbl = g_hash_table_lookup(self->functions_by_package, pkg);
    if (!tbl) {
      tbl = g_hash_table_new_full(g_str_hash, g_str_equal, g_free,
          (GDestroyNotify)function_unref);
      g_hash_table_insert(self->functions_by_package, g_strdup(pkg), tbl);
    }
    g_hash_table_replace(tbl, g_strdup(name), function_ref(function));
    LOG(1, "Index: added function %s to package %s", name, pkg);
  }
}

Function *project_index_get_function(ProjectIndex *self, const gchar *name) {
  g_return_val_if_fail(self != NULL, NULL);
  g_return_val_if_fail(name != NULL, NULL);
  return g_hash_table_lookup(self->functions, name);
}

gchar **project_index_get_function_names(ProjectIndex *self,
    const gchar *package, guint *length) {
  g_return_val_if_fail(self != NULL, NULL);
  g_return_val_if_fail(package != NULL, NULL);
  GHashTable *tbl = g_hash_table_lookup(self->functions_by_package, package);
  if (!tbl)
    return NULL;
  return (gchar**)g_hash_table_get_keys_as_array(tbl, length);
}

void project_index_add_variable(ProjectIndex *self, const gchar *package,
    const gchar *name, const gchar *doc) {
  g_return_if_fail(self != NULL);
  g_return_if_fail(package != NULL);
  g_return_if_fail(name != NULL);
  g_return_if_fail(glide_is_ui_thread());
  g_hash_table_replace(self->variables, g_strdup(name),
      doc ? g_strdup(doc) : NULL);
  LOG(1, "Index: added variable %s", name);
  GHashTable *tbl = g_hash_table_lookup(self->variables_by_package, package);
  if (!tbl) {
    tbl = g_hash_table_new_full(g_str_hash, g_str_equal, g_free, g_free);
    g_hash_table_insert(self->variables_by_package, g_strdup(package), tbl);
  }
  g_hash_table_replace(tbl, g_strdup(name), doc ? g_strdup(doc) : NULL);
  LOG(1, "Index: added variable %s to package %s", name, package);
}

const gchar *project_index_get_variable(ProjectIndex *self, const gchar *name) {
  g_return_val_if_fail(self != NULL, NULL);
  g_return_val_if_fail(name != NULL, NULL);
  return g_hash_table_lookup(self->variables, name);
}

gchar **project_index_get_variable_names(ProjectIndex *self,
    const gchar *package, guint *length) {
  g_return_val_if_fail(self != NULL, NULL);
  g_return_val_if_fail(package != NULL, NULL);
  GHashTable *tbl = g_hash_table_lookup(self->variables_by_package, package);
  if (!tbl)
    return NULL;
  return (gchar**)g_hash_table_get_keys_as_array(tbl, length);
}

