#ifndef SWANK_H
#define SWANK_H

#include <glib-object.h>

/* Type definition for Swank class */
#define SWANK_TYPE (swank_get_type())
G_DECLARE_FINAL_TYPE(Swank, swank, SWANK, OBJECT, GObject)

/* Public API */
Swank *swank_get_instance(void);  // Get the singleton instance
void swank_init_function(Swank *self);
void swank_remote_execution(Swank *self, const char *expr);

#endif // SWANK_H
