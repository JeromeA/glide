#include "string_text_provider.h"

static gsize stp_get_length(TextProvider *self);
static gunichar stp_get_char(TextProvider *self, gsize offset);
static gsize stp_next_offset(TextProvider *self, gsize offset);
static gchar *stp_get_text(TextProvider *self, gsize start, gsize end);
static void stp_destroy(TextProvider *self);

static const TextProviderOps stp_ops = {
  .get_length = stp_get_length,
  .get_char = stp_get_char,
  .next_offset = stp_next_offset,
  .get_text = stp_get_text,
  .destroy = stp_destroy,
};

TextProvider *string_text_provider_new(const gchar *text) {
  StringTextProvider *self = g_new0(StringTextProvider, 1);
  self->base.ops = &stp_ops;
  self->base.refcnt = 1;
  self->text = g_strdup(text);
  return (TextProvider*)self;
}

const gchar *string_text_provider_get_text_ref(StringTextProvider *self) {
  g_return_val_if_fail(self, NULL);
  return self->text;
}

static gsize stp_get_length(TextProvider *self) {
  StringTextProvider *tp = (StringTextProvider*)self;
  return strlen(tp->text);
}

static gunichar stp_get_char(TextProvider *self, gsize offset) {
  StringTextProvider *tp = (StringTextProvider*)self;
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
  StringTextProvider *tp = (StringTextProvider*)self;
  const gchar *p = tp->text + offset;
  const gchar *n = utf8_next_char(p);
  return (gsize)(n - tp->text);
}

static gchar *stp_get_text(TextProvider *self, gsize start, gsize end) {
  StringTextProvider *tp = (StringTextProvider*)self;
  return g_strndup(tp->text + start, end - start);
}

static void stp_destroy(TextProvider *self) {
  StringTextProvider *tp = (StringTextProvider*)self;
  g_free(tp->text);
  g_free(tp);
}

