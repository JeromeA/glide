#include "string_text_provider.h"

struct _StringTextProvider {
  GObject parent_instance;
  gchar *text;
};

static gsize stp_get_length(TextProvider *self);
static gunichar stp_get_char(TextProvider *self, gsize offset);
static gsize stp_next_offset(TextProvider *self, gsize offset);
static gchar *stp_get_text(TextProvider *self, gsize start, gsize end);

static void string_text_provider_text_provider_iface_init(TextProviderInterface *iface) {
  iface->get_length = stp_get_length;
  iface->get_char = stp_get_char;
  iface->next_offset = stp_next_offset;
  iface->get_text = stp_get_text;
}

G_DEFINE_TYPE_WITH_CODE(StringTextProvider, string_text_provider, G_TYPE_OBJECT,
    G_IMPLEMENT_INTERFACE(TEXT_PROVIDER_TYPE, string_text_provider_text_provider_iface_init))

static void string_text_provider_init(StringTextProvider *self) {
  self->text = NULL;
}

static void string_text_provider_finalize(GObject *object) {
  StringTextProvider *self = GLIDE_STRING_TEXT_PROVIDER(object);
  g_free(self->text);
  G_OBJECT_CLASS(string_text_provider_parent_class)->finalize(object);
}

static void string_text_provider_class_init(StringTextProviderClass *klass) {
  GObjectClass *obj = G_OBJECT_CLASS(klass);
  obj->finalize = string_text_provider_finalize;
}

TextProvider *string_text_provider_new(const gchar *text) {
  StringTextProvider *self = g_object_new(STRING_TEXT_PROVIDER_TYPE, NULL);
  self->text = g_strdup(text);
  return GLIDE_TEXT_PROVIDER(self);
}

const gchar *string_text_provider_get_text_ref(StringTextProvider *self) {
  g_return_val_if_fail(GLIDE_IS_STRING_TEXT_PROVIDER(self), NULL);
  return self->text;
}

static gsize stp_get_length(TextProvider *self) {
  StringTextProvider *tp = GLIDE_STRING_TEXT_PROVIDER(self);
  return strlen(tp->text);
}

static gunichar stp_get_char(TextProvider *self, gsize offset) {
  StringTextProvider *tp = GLIDE_STRING_TEXT_PROVIDER(self);
  const gchar *p = tp->text + offset;
  return g_utf8_get_char(p);
}

static const gchar *utf8_next_char(const gchar *p) {
  guchar c = (guchar)*p;
  if ((c & 0x80) == 0)
    return p + 1;
  if ((c & 0xe0) == 0xc0)
    return p + 2;
  if ((c & 0xf0) == 0xe0)
    return p + 3;
  if ((c & 0xf8) == 0xf0)
    return p + 4;
  if ((c & 0xfc) == 0xf8)
    return p + 5;
  if ((c & 0xfe) == 0xfc)
    return p + 6;
  return p + 1;
}

static gsize stp_next_offset(TextProvider *self, gsize offset) {
  StringTextProvider *tp = GLIDE_STRING_TEXT_PROVIDER(self);
  const gchar *p = tp->text + offset;
  const gchar *n = utf8_next_char(p);
  return (gsize)(n - tp->text);
}

static gchar *stp_get_text(TextProvider *self, gsize start, gsize end) {
  StringTextProvider *tp = GLIDE_STRING_TEXT_PROVIDER(self);
  return g_strndup(tp->text + start, end - start);
}

