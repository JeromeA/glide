#pragma once

#include <glib.h>
typedef struct _GtkTextBuffer GtkTextBuffer;
#include "text_provider.h"
#include "project_file.h"
#include "node.h"
#include "asdf.h"

typedef struct _Project Project;

typedef void (*ProjectFileLoadedCb)(Project *self, ProjectFile *file, gpointer user_data);
typedef void (*ProjectFileRemovedCb)(Project *self, ProjectFile *file, gpointer user_data);

Project       *project_new(void);
Project       *project_ref(Project *self);
void           project_unref(Project *self);
void           project_set_file_loaded_cb(Project *self, ProjectFileLoadedCb cb, gpointer user_data);
void           project_set_file_removed_cb(Project *self, ProjectFileRemovedCb cb, gpointer user_data);
ProjectFile   *project_create_scratch(Project *self);
ProjectFile   *project_get_file(Project *self, guint index);
guint          project_get_file_count(Project *self);
ProjectFile   *project_add_file(Project *self, TextProvider *provider,
    GtkTextBuffer *buffer, const gchar *path, ProjectFileState state);
void           project_remove_file(Project *self, ProjectFile *file);
void           project_file_changed(Project *self, ProjectFile *file);
void           project_file_loaded(Project *self, ProjectFile *file);
void           project_file_removed(Project *self, ProjectFile *file);
void           project_index_add(Project *self, Node *node);
GHashTable    *project_get_index(Project *self, StringDesignatorType sd_type);
void           project_set_asdf(Project *self, Asdf *asdf);
Asdf          *project_get_asdf(Project *self);
void           project_clear(Project *self);
const gchar   *project_get_path(Project *self);
void           project_set_path(Project *self, const gchar *path);
