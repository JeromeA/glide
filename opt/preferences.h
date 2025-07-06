
#ifndef PREFERENCES_H
#define PREFERENCES_H

#include <glib-object.h>

/* Type definition for Preferences class */
#define PREFERENCES_TYPE (preferences_get_type())
G_DECLARE_FINAL_TYPE(Preferences, preferences, GLIDE, PREFERENCES, GObject)

/* Public API */
Preferences *preferences_new(const gchar *config_dir);
const gchar *preferences_get_sdk(Preferences *self);
void preferences_set_sdk(Preferences *self, const gchar *new_sdk);
guint16 preferences_get_swank_port(Preferences *self);
void preferences_set_swank_port(Preferences *self, guint16 new_port);

#endif // PREFERENCES_H
