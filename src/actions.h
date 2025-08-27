#pragma once

#include <gtk/gtk.h>
#include "app.h"

G_BEGIN_DECLS

gboolean on_quit_delete_event(GtkWidget * /*widget*/, GdkEvent * /*event*/, gpointer data);
void on_quit_menu(GtkWidget * /*item*/, gpointer data);
void on_close_project(GtkWidget * /*item*/, gpointer data);
void on_save_all(GtkWidget * /*item*/, gpointer data);
void on_extend_selection(GtkWidget * /*item*/, gpointer data);
void on_shrink_selection(GtkWidget * /*item*/, gpointer data);
void on_recent_project(GtkWidget *item, gpointer data);
void on_show_parser(App *self);
gboolean on_key_press(GtkWidget * /*widget*/, GdkEventKey *event, gpointer user_data);
gboolean app_close_project(App *self, gboolean forget_project);

G_END_DECLS
