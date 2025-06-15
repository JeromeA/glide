#ifndef SWANK_SESSION_H
#define SWANK_SESSION_H

#include "swank_process.h"

#include <glib-object.h>

#define SWANK_SESSION_TYPE (swank_session_get_type())
G_DECLARE_FINAL_TYPE(SwankSession, swank_session, GLIDE, SWANK_SESSION, GObject)

SwankSession *swank_session_new(SwankProcessImpl *proc);
void swank_session_eval(SwankSession *self, const gchar *expr);

#endif /* SWANK_SESSION_H */
