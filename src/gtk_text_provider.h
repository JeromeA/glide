#pragma once

#include "text_provider.h"
#include <gtk/gtk.h>
#include <gtksourceview/gtksource.h>

#define GTK_TEXT_PROVIDER_TYPE (gtk_text_provider_get_type())
G_DECLARE_FINAL_TYPE(GtkTextProvider, gtk_text_provider, GLIDE, GTK_TEXT_PROVIDER, GObject)

TextProvider *gtk_text_provider_new(GtkTextBuffer *buffer);
GtkTextBuffer *gtk_text_provider_get_buffer(GtkTextProvider *self);

