#pragma once

#include <glib.h>

typedef struct _Preferences Preferences;

Preferences *preferences_new(const gchar *config_dir);
Preferences *preferences_ref(Preferences *self);
void         preferences_unref(Preferences *self);
const gchar *preferences_get_sdk(Preferences *self);
void         preferences_set_sdk(Preferences *self, const gchar *new_sdk);
const gchar *preferences_get_project_file(Preferences *self);
void         preferences_set_project_file(Preferences *self, const gchar *file);
const gchar *preferences_get_project_dir(Preferences *self);
void         preferences_set_project_dir(Preferences *self, const gchar *dir);
gint         preferences_get_project_view_width(Preferences *self);
void         preferences_set_project_view_width(Preferences *self, gint width);
gint         preferences_get_window_width(Preferences *self);
void         preferences_set_window_width(Preferences *self, gint width);
gint         preferences_get_window_height(Preferences *self);
void         preferences_set_window_height(Preferences *self, gint height);
const GList *preferences_get_recent_projects(Preferences *self);
void         preferences_add_recent_project(Preferences *self, const gchar *path);
const gchar *preferences_get_last_file(Preferences *self);
void         preferences_set_last_file(Preferences *self, const gchar *file);
gint         preferences_get_cursor_position(Preferences *self);
void         preferences_set_cursor_position(Preferences *self, gint pos);

