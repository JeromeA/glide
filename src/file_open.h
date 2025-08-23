#pragma once

#include <glib.h>
typedef struct _App App;

void file_open(GtkWidget *, gpointer data);
gboolean file_open_path(App *app, const gchar *filename);

