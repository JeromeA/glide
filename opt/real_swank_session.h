#ifndef REAL_SWANK_SESSION_H
#define REAL_SWANK_SESSION_H

#include "swank_session.h"
#include "swank_process.h"

#define REAL_SWANK_SESSION_TYPE (real_swank_session_get_type())
G_DECLARE_FINAL_TYPE(RealSwankSession, real_swank_session, GLIDE, REAL_SWANK_SESSION, GObject)

SwankSession *real_swank_session_new(SwankProcess *proc);
void real_swank_session_on_message(GString *msg, gpointer user_data);

#endif /* REAL_SWANK_SESSION_H */
