#include "text_provider.h"

TextProvider *text_provider_ref(TextProvider *self) {
  g_return_val_if_fail(self, NULL);
  self->refcnt++;
  return self;
}

void text_provider_unref(TextProvider *self) {
  if (!self)
    return;
  if (--self->refcnt == 0)
    self->ops->destroy(self);
}
