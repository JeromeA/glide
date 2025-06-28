#ifndef SWANK_SESSION_H
#define SWANK_SESSION_H

#include "swank_process.h"

#include <glib-object.h>

#define SWANK_SESSION_TYPE (swank_session_get_type())
G_DECLARE_INTERFACE(SwankSession, swank_session, GLIDE, SWANK_SESSION, GObject)

struct _SwankSessionInterface {
  GTypeInterface parent_iface;
  void (*eval)(SwankSession *self, const gchar *expr);
};

static inline void swank_session_eval(SwankSession *self, const gchar *expr) {
  g_return_if_fail(GLIDE_IS_SWANK_SESSION(self));
  GLIDE_SWANK_SESSION_GET_IFACE(self)->eval(self, expr);
}

#endif /* SWANK_SESSION_H */
