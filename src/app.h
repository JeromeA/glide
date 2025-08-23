#pragma once

#include <gtk/gtk.h>
#include <gtksourceview/gtksource.h>
#include "preferences.h"
#include "swank_session.h"
#include "project.h"
#include "lisp_source_view.h"
#include "lisp_source_notebook.h"
#include "status_service.h"

#ifndef STATIC
#define STATIC
#endif

G_BEGIN_DECLS

#define GLIDE_TYPE (app_get_type())
G_DECLARE_FINAL_TYPE(App, app, GLIDE, APP, GtkApplication)

STATIC App *app_new (Preferences *prefs, SwankSession *swank, Project *project, StatusService *status_service);
STATIC LispSourceView *app_get_source_view(App *self);
STATIC LispSourceNotebook *app_get_notebook(App *self);
STATIC Project *app_get_project(App *self);
STATIC void app_connect_view(App *self, LispSourceView *view);
STATIC void app_update_asdf_view(App *self);
STATIC void app_update_recent_menu(App *self);
STATIC Preferences *app_get_preferences(App *self);
STATIC SwankSession *app_get_swank(App *self);
STATIC void app_on_quit(App *self);
STATIC void app_quit(App *self);
STATIC StatusService *app_get_status_service(App *self);

G_END_DECLS

