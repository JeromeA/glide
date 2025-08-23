#pragma once

#include <glib.h>

typedef struct Package Package;

struct Package {
  gint ref;
  gchar *name;
  gchar *description;
  GHashTable *nicknames;
  GHashTable *uses;
  GHashTable *exports;
  GHashTable *shadows;
  GHashTable *import_from; /* symbol -> package */
};

Package *package_new(const gchar *name);
Package *package_ref(Package *package);
void package_unref(Package *package);
void package_set_description(Package *package, const gchar *description);
void package_add_nickname(Package *package, const gchar *nickname);
void package_add_use(Package *package, const gchar *use);
void package_add_export(Package *package, const gchar *symbol);
void package_add_shadow(Package *package, const gchar *symbol);
void package_add_import_from(Package *package, const gchar *symbol, const gchar *from);

