#pragma once

#include "preferences.h"
#include "process.h"

#include <glib.h>

typedef struct _SwankProcess SwankProcess;
typedef void (*SwankProcessMessageCallback)(GString *msg, gpointer user_data);

typedef struct {
  void (*start)(SwankProcess *self);
  void (*send)(SwankProcess *self, const GString *payload);
  void (*set_message_cb)(SwankProcess *self, SwankProcessMessageCallback cb,
                         gpointer user_data);
  void (*destroy)(SwankProcess *self);
} SwankProcessOps;

struct _SwankProcess {
  const SwankProcessOps *ops;
  int refcnt;
};

static inline void swank_process_send(SwankProcess *self, const GString *payload) {
  g_return_if_fail(self);
  self->ops->send(self, payload);
}

static inline void swank_process_start(SwankProcess *self) {
  g_return_if_fail(self);
  self->ops->start(self);
}

static inline void swank_process_set_message_cb(SwankProcess *self,
    SwankProcessMessageCallback cb,
    gpointer user_data) {
  g_return_if_fail(self);
  self->ops->set_message_cb(self, cb, user_data);
}

SwankProcess *swank_process_ref(SwankProcess *self);
void          swank_process_unref(SwankProcess *self);

