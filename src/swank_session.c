#include "swank_session.h"

SwankSession *swank_session_ref(SwankSession *self) {
  g_return_val_if_fail(self, NULL);
  self->refcnt++;
  return self;
}

void swank_session_unref(SwankSession *self) {
  if (!self)
    return;
  if (--self->refcnt == 0)
    self->ops->destroy(self);
}
