#ifndef GLIDE_H
#define GLIDE_H

#include <gtk/gtk.h>
#include <gtksourceview/gtksource.h>
#include "preferences.h"
#include "swank_session.h"
#include "project.h"

#ifndef STATIC
#define STATIC
#endif

G_BEGIN_DECLS

#define GLIDE_TYPE (app_get_type())
G_DECLARE_FINAL_TYPE(App, app, GLIDE, APP, GtkApplication)

STATIC App *app_new (Preferences *prefs, SwankSession *swank, Project *project);
STATIC GtkSourceBuffer *app_get_source_buffer (App *self);
STATIC const gchar *app_get_filename  (App *self); // Returns the current filename or NULL (borrowed – do not free)
STATIC void app_set_filename  (App *self, const gchar *new_filename);
STATIC Preferences *app_get_preferences(App *self);
STATIC SwankSession *app_get_swank(App *self);
STATIC void app_on_quit(App *self);
STATIC void app_quit(App *self);

G_END_DECLS

#endif /* GLIDE_H */

