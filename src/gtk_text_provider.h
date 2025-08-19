#pragma once

#include "text_provider.h"
#include <gtk/gtk.h>
#include <gtksourceview/gtksource.h>

typedef struct {
  TextProvider base;
  GtkTextBuffer *buffer;
} GtkTextProvider;

TextProvider *gtk_text_provider_new(GtkTextBuffer *buffer);
GtkTextBuffer *gtk_text_provider_get_buffer(GtkTextProvider *self);

