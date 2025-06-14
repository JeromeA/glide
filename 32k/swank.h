#ifndef SWANK_H
#define SWANK_H

#include <glib-object.h>
#include "preferences.h"

#ifndef STATIC
#define STATIC
#endif

/* Type definition for Swank class */
#define SWANK_TYPE (swank_get_type())
G_DECLARE_FINAL_TYPE(Swank, swank, GLIDE, SWANK, GObject)

/* Public API */
STATIC Swank *swank_new(Preferences *prefs);  // Create a new Swank instance
STATIC void  swank_remote_execution(Swank *self, const char *expr);

#endif // SWANK_H
