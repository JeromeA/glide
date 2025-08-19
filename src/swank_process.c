#include "swank_process.h"

SwankProcess *swank_process_ref(SwankProcess *self) {
  g_return_val_if_fail(self, NULL);
  self->refcnt++;
  return self;
}

void swank_process_unref(SwankProcess *self) {
  if (!self)
    return;
  if (--self->refcnt == 0)
    self->ops->destroy(self);
}
