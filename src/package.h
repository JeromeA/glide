#pragma once

#include <glib.h>

typedef struct Package Package;

Package *package_new(const gchar *name);
Package *package_ref(Package *package);
void package_unref(Package *package);
void package_set_description(Package *package, const gchar *description);
void package_add_nickname(Package *package, const gchar *nickname);
void package_add_use(Package *package, const gchar *use);
void package_add_export(Package *package, const gchar *symbol);
void package_add_shadow(Package *package, const gchar *symbol);
void package_add_import_from(Package *package, const gchar *symbol, const gchar *from);

const gchar *package_get_name(const Package *package);
const gchar *package_get_description(const Package *package);
GHashTable *package_get_nicknames(const Package *package);
GHashTable *package_get_uses(const Package *package);
GHashTable *package_get_exports(const Package *package);
GHashTable *package_get_shadows(const Package *package);
GHashTable *package_get_import_from(const Package *package);

