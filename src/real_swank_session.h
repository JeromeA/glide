#ifndef REAL_SWANK_SESSION_H
#define REAL_SWANK_SESSION_H

#include "swank_session.h"
#include "swank_process.h"

typedef struct {
  SwankSession base;
  SwankProcess *proc;
  gboolean started;
  guint32 next_tag;
  GHashTable *interactions;
  SwankSessionCallback added_cb;
  gpointer added_cb_data;
  SwankSessionCallback updated_cb;
  gpointer updated_cb_data;
} RealSwankSession;

SwankSession *real_swank_session_new(SwankProcess *proc);
void real_swank_session_on_message(GString *msg, gpointer user_data);

#endif /* REAL_SWANK_SESSION_H */
