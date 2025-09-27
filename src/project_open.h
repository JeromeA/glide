#pragma once

#include <glib.h>
typedef struct _App App;

void project_open(GtkWidget *, gpointer data);
gboolean project_open_path(App *app, const gchar *filename);

