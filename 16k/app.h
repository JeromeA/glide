#ifndef GLIDE_H
#define GLIDE_H

#include <gtk/gtk.h>
#include <gtksourceview/gtksource.h>
#include "preferences.h"

#ifndef STATIC
#define STATIC
#endif

G_BEGIN_DECLS

#define GLIDE_TYPE (app_get_type())
G_DECLARE_FINAL_TYPE(App, app, GLIDE, APP, GtkApplication)

STATIC App *app_new (const gchar *prefs_filename);
STATIC GtkSourceBuffer *app_get_source_buffer (App *self);
STATIC const gchar *app_get_filename  (App *self); // Returns the current filename or NULL (borrowed – do not free)
STATIC void app_set_filename  (App *self, const gchar *new_filename);
STATIC Preferences *app_get_preferences(App *self);

G_END_DECLS

#endif /* GLIDE_H */

