#pragma once

#include <gtk/gtk.h>
#include <gtksourceview/gtksource.h>
#include "preferences.h"
#include "repl_session.h"
#include "project.h"
#include "editor.h"
#include "editor_container.h"
#include "status_service.h"
#include "editor_manager.h"

#ifndef STATIC
#define STATIC
#endif

G_BEGIN_DECLS

#define GLIDE_TYPE (app_get_type())
G_DECLARE_FINAL_TYPE(App, app, GLIDE, APP, GtkApplication)

STATIC App *app_new (Preferences *prefs, ReplSession *glide, StatusService *status_service);
STATIC Editor *app_get_editor(App *self);
STATIC EditorContainer *app_get_editor_container(App *self);
STATIC EditorManager *app_get_editor_manager(App *self);
STATIC Project *app_get_project(App *self);
STATIC void app_connect_editor(App *self, Editor *editor);
STATIC ProjectFile *app_get_current_file(App *self);
STATIC void app_update_project_view(App *self);
STATIC void app_restore_last_file(App *self);
STATIC void app_update_recent_menu(App *self);
STATIC Preferences *app_get_preferences(App *self);
STATIC ReplSession *app_get_glide(App *self);
STATIC void app_on_quit(App *self);
STATIC void app_quit(App *self);
void app_set_recent_menu(App *self, GMenu *menu);

G_END_DECLS

