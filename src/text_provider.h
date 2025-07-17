#ifndef TEXT_PROVIDER_H
#define TEXT_PROVIDER_H

#include <glib-object.h>

#define TEXT_PROVIDER_TYPE (text_provider_get_type())
G_DECLARE_INTERFACE(TextProvider, text_provider, GLIDE, TEXT_PROVIDER, GObject)

struct _TextProviderInterface {
  GTypeInterface parent_iface;
  gsize   (*get_length)(TextProvider *self);
  gunichar (*get_char)(TextProvider *self, gsize offset);
  gsize   (*next_offset)(TextProvider *self, gsize offset);
  gchar  *(*get_text)(TextProvider *self, gsize start, gsize end);
};

static inline gsize text_provider_get_length(TextProvider *self) {
  g_return_val_if_fail(GLIDE_IS_TEXT_PROVIDER(self), 0);
  return GLIDE_TEXT_PROVIDER_GET_IFACE(self)->get_length(self);
}

static inline gunichar text_provider_get_char(TextProvider *self, gsize offset) {
  g_return_val_if_fail(GLIDE_IS_TEXT_PROVIDER(self), 0);
  return GLIDE_TEXT_PROVIDER_GET_IFACE(self)->get_char(self, offset);
}

static inline gsize text_provider_next_offset(TextProvider *self, gsize offset) {
  g_return_val_if_fail(GLIDE_IS_TEXT_PROVIDER(self), offset);
  return GLIDE_TEXT_PROVIDER_GET_IFACE(self)->next_offset(self, offset);
}

static inline gchar *text_provider_get_text(TextProvider *self, gsize start, gsize end) {
  g_return_val_if_fail(GLIDE_IS_TEXT_PROVIDER(self), NULL);
  return GLIDE_TEXT_PROVIDER_GET_IFACE(self)->get_text(self, start, end);
}

#endif /* TEXT_PROVIDER_H */
