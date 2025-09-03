#include "project_index.h"
#include "node.h"
#include "package.h"
#include "function.h"
#include "project_file.h"

struct _ProjectIndex {
  GHashTable *function_defs; /* name -> GPtrArray* Node* */
  GHashTable *function_uses;
  GHashTable *variable_defs;
  GHashTable *variable_uses;
  GHashTable *package_defs;
  GHashTable *package_uses;
  GHashTable *packages; /* name -> Package* */
  GHashTable *functions; /* name -> Function* */
};

static GHashTable *project_index_table(ProjectIndex *self, StringDesignatorType sd_type);
static void project_index_add_to(GHashTable *table, const gchar *name, Node *node);
static void project_index_node(ProjectIndex *self, const Node *node);
static void project_index_remove_from_table(GHashTable *table, ProjectFile *file);

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
  return self;
}

void project_index_free(ProjectIndex *self) {
  if (!self) return;
  project_index_clear(self);
  g_clear_pointer(&self->function_defs, g_hash_table_unref);
  g_clear_pointer(&self->function_uses, g_hash_table_unref);
  g_clear_pointer(&self->variable_defs, g_hash_table_unref);
  g_clear_pointer(&self->variable_uses, g_hash_table_unref);
  g_clear_pointer(&self->package_defs, g_hash_table_unref);
  g_clear_pointer(&self->package_uses, g_hash_table_unref);
  g_clear_pointer(&self->packages, g_hash_table_unref);
  g_clear_pointer(&self->functions, g_hash_table_unref);
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
}

static void project_index_node(ProjectIndex *self, const Node *node) {
  if (!node || !node->sd_type) return;
  const gchar *name = node_get_name(node);
  GHashTable *table = project_index_table(self, node->sd_type);
  project_index_add_to(table, name, (Node*)node);
}

void project_index_walk(ProjectIndex *self, const Node *node) {
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
  GHashTable *tables[] = { self->function_defs, self->function_uses,
    self->variable_defs, self->variable_uses, self->package_defs,
    self->package_uses };
  for (guint t = 0; t < G_N_ELEMENTS(tables); t++)
    if (tables[t])
      g_hash_table_remove_all(tables[t]);
  if (self->packages)
    g_hash_table_remove_all(self->packages);
  if (self->functions)
    g_hash_table_remove_all(self->functions);
}

static void project_index_remove_from_table(GHashTable *table, ProjectFile *file) {
  if (!table) return;
  GHashTableIter iter;
  g_hash_table_iter_init(&iter, table);
  gpointer key, value;
  while (g_hash_table_iter_next(&iter, &key, &value)) {
    GPtrArray *arr = value;
    for (guint i = 0; i < arr->len; ) {
      Node *n = g_ptr_array_index(arr, i);
      if (n->file == file)
        g_ptr_array_remove_index(arr, i);
      else
        i++;
    }
    if (arr->len == 0)
      g_hash_table_iter_remove(&iter);
  }
}

void project_index_remove_file(ProjectIndex *self, ProjectFile *file) {
  GHashTable *tables[] = { self->function_defs, self->function_uses,
    self->variable_defs, self->variable_uses, self->package_defs,
    self->package_uses };
  for (guint t = 0; t < G_N_ELEMENTS(tables); t++)
    project_index_remove_from_table(tables[t], file);

  if (self->functions) {
    GHashTableIter iter;
    g_hash_table_iter_init(&iter, self->functions);
    gpointer key, value;
    while (g_hash_table_iter_next(&iter, &key, &value)) {
      Function *fn = value;
      const Node *sym = function_get_symbol(fn);
      if (sym && sym->file == file)
        g_hash_table_iter_remove(&iter);
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
        if (n->file == file) {
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
  const gchar *name = package_get_name(package);
  if (!name) return;
  g_hash_table_replace(self->packages, g_strdup(name), package_ref(package));
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
  const gchar *name = function_get_name(function);
  if (!name) return;
  g_hash_table_replace(self->functions, g_strdup(name), function_ref(function));
}

Function *project_index_get_function(ProjectIndex *self, const gchar *name) {
  g_return_val_if_fail(self != NULL, NULL);
  g_return_val_if_fail(name != NULL, NULL);
  return g_hash_table_lookup(self->functions, name);
}

