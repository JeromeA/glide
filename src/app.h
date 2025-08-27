#pragma once

#include <gtk/gtk.h>
#include <gtksourceview/gtksource.h>
#include "preferences.h"
#include "swank_session.h"
#include "project.h"
#include "editor.h"
#include "lisp_source_notebook.h"
#include "status_service.h"

#ifndef STATIC
#define STATIC
#endif

G_BEGIN_DECLS

#define GLIDE_TYPE (app_get_type())
G_DECLARE_FINAL_TYPE(App, app, GLIDE, APP, GtkApplication)

STATIC App *app_new (Preferences *prefs, SwankSession *swank, Project *project, StatusService *status_service);
STATIC Editor *app_get_editor(App *self);
STATIC LispSourceNotebook *app_get_notebook(App *self);
STATIC Project *app_get_project(App *self);
STATIC void app_connect_editor(App *self, Editor *editor);
STATIC ProjectFile *app_get_current_file(App *self);
STATIC void app_update_asdf_view(App *self);
STATIC void app_update_recent_menu(App *self);
STATIC Preferences *app_get_preferences(App *self);
STATIC SwankSession *app_get_swank(App *self);
STATIC void app_on_quit(App *self);
STATIC void app_quit(App *self);
STATIC StatusService *app_get_status_service(App *self);
void app_set_recent_menu(App *self, GtkWidget *menu);

G_END_DECLS

