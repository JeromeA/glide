#pragma once

#include "text_provider.h"

#define STRING_TEXT_PROVIDER_TYPE (string_text_provider_get_type())
G_DECLARE_FINAL_TYPE(StringTextProvider, string_text_provider, GLIDE, STRING_TEXT_PROVIDER, GObject)

TextProvider *string_text_provider_new(const gchar *text);
const gchar *string_text_provider_get_text_ref(StringTextProvider *self);

