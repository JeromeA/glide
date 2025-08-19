#ifndef TEXT_PROVIDER_H
#define TEXT_PROVIDER_H

#include <glib.h>

typedef struct _TextProvider TextProvider;

typedef struct {
  gsize   (*get_length)(TextProvider *self);
  gunichar (*get_char)(TextProvider *self, gsize offset);
  gsize   (*next_offset)(TextProvider *self, gsize offset);
  gchar  *(*get_text)(TextProvider *self, gsize start, gsize end);
  void    (*destroy)(TextProvider *self);
} TextProviderOps;

struct _TextProvider {
  const TextProviderOps *ops;
  int refcnt;
};

static inline gsize text_provider_get_length(TextProvider *self) {
  g_return_val_if_fail(self, 0);
  return self->ops->get_length(self);
}

static inline gunichar text_provider_get_char(TextProvider *self, gsize offset) {
  g_return_val_if_fail(self, 0);
  return self->ops->get_char(self, offset);
}

static inline gsize text_provider_next_offset(TextProvider *self, gsize offset) {
  g_return_val_if_fail(self, offset);
  return self->ops->next_offset(self, offset);
}

static inline gchar *text_provider_get_text(TextProvider *self, gsize start, gsize end) {
  g_return_val_if_fail(self, NULL);
  return self->ops->get_text(self, start, end);
}

TextProvider *text_provider_ref(TextProvider *self);
void          text_provider_unref(TextProvider *self);

#endif /* TEXT_PROVIDER_H */
