#pragma once

#include <glib.h>

typedef struct _Preferences Preferences;

Preferences *preferences_new(const gchar *config_dir);
Preferences *preferences_ref(Preferences *self);
void         preferences_unref(Preferences *self);
const gchar *preferences_get_sdk(Preferences *self);
void         preferences_set_sdk(Preferences *self, const gchar *new_sdk);
guint16      preferences_get_swank_port(Preferences *self);
void         preferences_set_swank_port(Preferences *self, guint16 new_port);
const gchar *preferences_get_project_file(Preferences *self);
void         preferences_set_project_file(Preferences *self, const gchar *file);

