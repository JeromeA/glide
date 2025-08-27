#pragma once

#include "glide_process.h"
#include "interaction.h"

#include <glib.h>

typedef struct _GlideSession GlideSession;
typedef void (*GlideSessionCallback)(GlideSession *self, Interaction *interaction, gpointer user_data);

typedef struct {
  void (*eval)(GlideSession *self, Interaction *interaction);
  void (*set_interaction_added_cb)(GlideSession *self, GlideSessionCallback cb, gpointer user_data);
  void (*set_interaction_updated_cb)(GlideSession *self, GlideSessionCallback cb, gpointer user_data);
  void (*destroy)(GlideSession *self);
} GlideSessionOps;

struct _GlideSession {
  const GlideSessionOps *ops;
  int refcnt;
};

static inline void glide_session_eval(GlideSession *self, Interaction *interaction) {
  g_return_if_fail(self);
  self->ops->eval(self, interaction);
}

static inline void glide_session_set_interaction_added_cb(GlideSession *self, GlideSessionCallback cb, gpointer user_data) {
  g_return_if_fail(self);
  self->ops->set_interaction_added_cb(self, cb, user_data);
}

static inline void glide_session_set_interaction_updated_cb(GlideSession *self, GlideSessionCallback cb, gpointer user_data) {
  g_return_if_fail(self);
  self->ops->set_interaction_updated_cb(self, cb, user_data);
}

GlideSession *glide_session_ref(GlideSession *self);
void glide_session_unref(GlideSession *self);

