#include "package.h"

Package *package_new(const gchar *name) {
  Package *package = g_new0(Package, 1);
  g_atomic_int_set(&package->ref, 1);
  package->name = g_strdup(name);
  package->nicknames = g_hash_table_new_full(g_str_hash, g_str_equal, g_free, NULL);
  package->uses = g_hash_table_new_full(g_str_hash, g_str_equal, g_free, NULL);
  package->exports = g_hash_table_new_full(g_str_hash, g_str_equal, g_free, NULL);
  package->shadows = g_hash_table_new_full(g_str_hash, g_str_equal, g_free, NULL);
  package->import_from = g_hash_table_new_full(g_str_hash, g_str_equal, g_free, g_free);
  return package;
}

Package *package_ref(Package *package) {
  if (!package) return NULL;
  g_atomic_int_inc(&package->ref);
  return package;
}

static void package_finalize(Package *package) {
  g_clear_pointer(&package->name, g_free);
  g_clear_pointer(&package->description, g_free);
  g_clear_pointer(&package->nicknames, g_hash_table_unref);
  g_clear_pointer(&package->uses, g_hash_table_unref);
  g_clear_pointer(&package->exports, g_hash_table_unref);
  g_clear_pointer(&package->shadows, g_hash_table_unref);
  g_clear_pointer(&package->import_from, g_hash_table_unref);
}

void package_unref(Package *package) {
  if (!package) return;
  if (g_atomic_int_dec_and_test(&package->ref)) {
    package_finalize(package);
    g_free(package);
  }
}

void package_set_description(Package *package, const gchar *description) {
  if (!package) return;
  g_free(package->description);
  package->description = description ? g_strdup(description) : NULL;
}

static void package_add_to_set(GHashTable *set, const gchar *value) {
  if (set && value)
    g_hash_table_add(set, g_strdup(value));
}

void package_add_nickname(Package *package, const gchar *nickname) {
  if (!package) return;
  package_add_to_set(package->nicknames, nickname);
}

void package_add_use(Package *package, const gchar *use) {
  if (!package) return;
  package_add_to_set(package->uses, use);
}

void package_add_export(Package *package, const gchar *symbol) {
  if (!package) return;
  package_add_to_set(package->exports, symbol);
}

void package_add_shadow(Package *package, const gchar *symbol) {
  if (!package) return;
  package_add_to_set(package->shadows, symbol);
}

void package_add_import_from(Package *package, const gchar *symbol, const gchar *from) {
  if (!package || !symbol || !from) return;
  g_hash_table_insert(package->import_from, g_strdup(symbol), g_strdup(from));
}
