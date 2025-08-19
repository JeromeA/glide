#include "process.h"

Process *process_ref(Process *self) {
  g_return_val_if_fail(self, NULL);
  self->refcnt++;
  return self;
}

void process_unref(Process *self) {
  if (!self)
    return;
  if (--self->refcnt == 0)
    self->ops->destroy(self);
}
