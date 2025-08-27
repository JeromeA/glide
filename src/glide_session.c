#include "glide_session.h"

GlideSession *glide_session_ref(GlideSession *self) {
  g_return_val_if_fail(self, NULL);
  self->refcnt++;
  return self;
}

void glide_session_unref(GlideSession *self) {
  if (!self)
    return;
  if (--self->refcnt == 0)
    self->ops->destroy(self);
}
