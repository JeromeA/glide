#ifndef SWANK_SESSION_H
#define SWANK_SESSION_H

#include "swank_process.h"
#include "interaction.h"

#include <glib.h>

typedef struct _SwankSession SwankSession;
typedef void (*SwankSessionCallback)(SwankSession *self, Interaction *interaction, gpointer user_data);

typedef struct {
  void (*eval)(SwankSession *self, Interaction *interaction);
  void (*set_interaction_added_cb)(SwankSession *self, SwankSessionCallback cb, gpointer user_data);
  void (*set_interaction_updated_cb)(SwankSession *self, SwankSessionCallback cb, gpointer user_data);
  void (*destroy)(SwankSession *self);
} SwankSessionOps;

struct _SwankSession {
  const SwankSessionOps *ops;
  int refcnt;
};

static inline void swank_session_eval(SwankSession *self, Interaction *interaction) {
  g_return_if_fail(self);
  self->ops->eval(self, interaction);
}

static inline void swank_session_set_interaction_added_cb(SwankSession *self, SwankSessionCallback cb, gpointer user_data) {
  g_return_if_fail(self);
  self->ops->set_interaction_added_cb(self, cb, user_data);
}

static inline void swank_session_set_interaction_updated_cb(SwankSession *self, SwankSessionCallback cb, gpointer user_data) {
  g_return_if_fail(self);
  self->ops->set_interaction_updated_cb(self, cb, user_data);
}

SwankSession *swank_session_ref(SwankSession *self);
void swank_session_unref(SwankSession *self);

#endif /* SWANK_SESSION_H */
