
#ifndef SETTINGS_H
#define SETTINGS_H

#include <glib-object.h>

/* Type definition for Settings class */
#define SETTINGS_TYPE (settings_get_type())
G_DECLARE_FINAL_TYPE(Settings, settings, SETTINGS, CLASS, GObject)

/* Public API */
Settings *settings_get_instance(void);
const gchar *settings_get_sdk(Settings *self);
void settings_set_sdk(Settings *self, const gchar *new_sdk);

#endif // SETTINGS_H
