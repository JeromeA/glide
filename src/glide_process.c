#include "glide_process.h"

GlideProcess *glide_process_ref(GlideProcess *self) {
  g_return_val_if_fail(self, NULL);
  self->refcnt++;
  return self;
}

void glide_process_unref(GlideProcess *self) {
  if (!self)
    return;
  if (--self->refcnt == 0)
    self->ops->destroy(self);
}
