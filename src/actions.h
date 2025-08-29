#pragma once

#include <gtk/gtk.h>
#include "app.h"

G_BEGIN_DECLS

gboolean on_quit_delete_event(GtkWidget * /*widget*/, GdkEvent * /*event*/, gpointer data);
void actions_init(App *self);
gboolean app_close_project(App *self, gboolean forget_project);

G_END_DECLS
