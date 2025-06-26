#ifndef SWANK_PROCESS_H
#define SWANK_PROCESS_H

#include "preferences.h"
#include "process.h"

#include <glib-object.h>

#define SWANK_PROCESS_TYPE (swank_process_get_type())
G_DECLARE_INTERFACE(SwankProcess, swank_process, GLIDE, SWANK_PROCESS, GObject)

struct _SwankProcessInterface {
  GTypeInterface parent_iface;
  void     (*start)(SwankProcess *self);
  void     (*send)(SwankProcess *self, const GString *payload);
  GString *(*get_reply)(SwankProcess *self);
};

static inline void swank_process_send(SwankProcess *self, const GString *payload) {
  g_return_if_fail(GLIDE_IS_SWANK_PROCESS(self));
  GLIDE_SWANK_PROCESS_GET_IFACE(self)->send(self, payload);
}

static inline void swank_process_start(SwankProcess *self) {
  g_return_if_fail(GLIDE_IS_SWANK_PROCESS(self));
  GLIDE_SWANK_PROCESS_GET_IFACE(self)->start(self);
}

static inline GString *swank_process_get_reply(SwankProcess *self) {
  g_return_val_if_fail(GLIDE_IS_SWANK_PROCESS(self), NULL);
  return GLIDE_SWANK_PROCESS_GET_IFACE(self)->get_reply(self);
}

#endif /* SWANK_PROCESS_H */
