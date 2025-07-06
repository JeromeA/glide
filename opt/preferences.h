
#ifndef PREFERENCES_H
#define PREFERENCES_H

#include <glib.h> // For gchar, guint16 etc. No longer a GObject.

/* Public API for managing global preferences */
void preferences_init_globals(const gchar *config_dir);
const gchar *preferences_get_sdk_global();
void preferences_set_sdk_global(const gchar *new_sdk);
guint16 preferences_get_swank_port_global();
void preferences_set_swank_port_global(guint16 new_port);
void preferences_save_globals(); // To save all current global values
void preferences_cleanup_globals(); // Declaration for cleanup function

// The filename is managed internally after init.
// No need for get/set for filename_global from outside this unit after init.

#endif // PREFERENCES_H
