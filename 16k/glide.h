#ifndef GLIDE_H
#define GLIDE_H

#include <gtk/gtk.h>
#include <gtksourceview/gtksource.h>

#ifndef STATIC
#define STATIC
#endif

G_BEGIN_DECLS

#define GLIDE_TYPE (glide_get_type())
G_DECLARE_FINAL_TYPE(Glide, glide, GLIDE, APP, GtkApplication)

STATIC Glide *glide_new (void);
STATIC GtkSourceBuffer *glide_get_source_buffer (Glide *self);
STATIC const gchar *glide_get_filename  (Glide *self); // Returns the current filename or NULL (borrowed – do not free)
STATIC void glide_set_filename  (Glide *self, const gchar *new_filename);

G_END_DECLS

#endif /* GLIDE_H */

