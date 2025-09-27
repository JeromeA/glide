#pragma once

#include <glib-object.h>

G_BEGIN_DECLS

#define ASDF_TYPE (asdf_get_type())
G_DECLARE_FINAL_TYPE(Asdf, asdf, GLIDE, ASDF, GObject)

Asdf *asdf_new(void);
Asdf *asdf_new_from_file(GString *filename);
const GString *asdf_get_filename(Asdf *self);
void asdf_set_description(Asdf *self, GString *description);
const GString *asdf_get_description(Asdf *self);
void asdf_set_serial(Asdf *self, gboolean serial);
gboolean asdf_get_serial(Asdf *self);
void asdf_add_component(Asdf *self, GString *file);
const GString *asdf_get_component(Asdf *self, guint index);
guint asdf_get_component_count(Asdf *self);
void asdf_remove_component(Asdf *self, const GString *file);
gboolean asdf_rename_component(Asdf *self, const GString *old_file, GString *new_file);
void asdf_add_dependency(Asdf *self, GString *dep);
const GString *asdf_get_dependency(Asdf *self, guint index);
guint asdf_get_dependency_count(Asdf *self);
GString *asdf_to_string(Asdf *self);
gboolean asdf_save(Asdf *self, const GString *filename);

G_END_DECLS
