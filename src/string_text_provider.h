#pragma once

#include "text_provider.h"

typedef struct {
  TextProvider base;
  gchar *text;
} StringTextProvider;

TextProvider *string_text_provider_new(const gchar *text);
const gchar *string_text_provider_get_text_ref(StringTextProvider *self);

