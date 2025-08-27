#pragma once

#include "preferences.h"
#include "process.h"

#include <glib.h>

typedef struct _GlideProcess GlideProcess;
typedef void (*GlideProcessMessageCallback)(GString *msg, gpointer user_data);

typedef struct {
  void (*start)(GlideProcess *self);
  void (*send)(GlideProcess *self, const GString *payload);
  void (*set_message_cb)(GlideProcess *self, GlideProcessMessageCallback cb,
                         gpointer user_data);
  void (*destroy)(GlideProcess *self);
} GlideProcessOps;

struct _GlideProcess {
  const GlideProcessOps *ops;
  int refcnt;
};

static inline void glide_process_send(GlideProcess *self, const GString *payload) {
  g_return_if_fail(self);
  self->ops->send(self, payload);
}

static inline void glide_process_start(GlideProcess *self) {
  g_return_if_fail(self);
  self->ops->start(self);
}

static inline void glide_process_set_message_cb(GlideProcess *self,
    GlideProcessMessageCallback cb,
    gpointer user_data) {
  g_return_if_fail(self);
  self->ops->set_message_cb(self, cb, user_data);
}

GlideProcess *glide_process_ref(GlideProcess *self);
void          glide_process_unref(GlideProcess *self);

