#pragma once

#include <glib-object.h>

G_BEGIN_DECLS

#define ASDF_TYPE (asdf_get_type())
G_DECLARE_FINAL_TYPE(Asdf, asdf, GLIDE, ASDF, GObject)

Asdf *asdf_new(void);
Asdf *asdf_new_from_file(const gchar *filename);
const gchar *asdf_get_filename(Asdf *self);
const gchar *asdf_get_pathname(Asdf *self);
void asdf_set_pathname(Asdf *self, const gchar *pathname);
void asdf_set_serial(Asdf *self, gboolean serial);
gboolean asdf_get_serial(Asdf *self);
void asdf_add_component(Asdf *self, const gchar *file);
const gchar *asdf_get_component(Asdf *self, guint index);
guint asdf_get_component_count(Asdf *self);
void asdf_add_dependency(Asdf *self, const gchar *dep);
const gchar *asdf_get_dependency(Asdf *self, guint index);
guint asdf_get_dependency_count(Asdf *self);
char *asdf_to_string(Asdf *self);
gboolean asdf_save(Asdf *self, const gchar *filename);

G_END_DECLS
