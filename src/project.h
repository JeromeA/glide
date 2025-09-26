#pragma once

#include <glib.h>
#include "project_file.h"
#include "node.h"
#include "asdf.h"
#include "package.h"
#include "function.h"

typedef struct _Project Project;
typedef struct _ReplSession ReplSession;

typedef void (*ProjectFileLoadedCb)(Project *self, ProjectFile *file, gpointer user_data);
typedef void (*ProjectFileRemovedCb)(Project *self, ProjectFile *file, gpointer user_data);
typedef void (*ProjectChangedCb)(Project *self, gpointer user_data);
typedef void (*ProjectFileChangedCb)(Project *self, ProjectFile *file, gpointer user_data);

Project       *project_new(ReplSession *repl);
Project       *project_ref(Project *self);
void           project_unref(Project *self);
void           project_set_file_loaded_cb(Project *self, ProjectFileLoadedCb cb, gpointer user_data);
void           project_set_file_removed_cb(Project *self, ProjectFileRemovedCb cb, gpointer user_data);
void           project_set_file_changed_cb(Project *self, ProjectFileChangedCb cb, gpointer user_data);
void           project_set_changed_cb(Project *self, ProjectChangedCb cb, gpointer user_data);
ProjectFile   *project_get_file(Project *self, guint index);
guint          project_get_file_count(Project *self);
ProjectFile   *project_add_file(Project *self, GString *content,
    const gchar *path, ProjectFileState state);
ProjectFile   *project_add_loaded_file(Project *self, const gchar *path);
void           project_remove_file(Project *self, ProjectFile *file);
void           project_file_changed(Project *self, ProjectFile *file);
void           project_file_loaded(Project *self, ProjectFile *file);
void           project_file_removed(Project *self, ProjectFile *file);
GHashTable    *project_get_index(Project *self, StringDesignatorType sd_type);
void           project_add_package(Project *self, Package *package);
Package       *project_get_package(Project *self, const gchar *name);
gchar        **project_get_package_names(Project *self, guint *length);
void           project_set_asdf(Project *self, Asdf *asdf);
Asdf          *project_get_asdf(Project *self);
void           project_clear(Project *self);
const gchar   *project_get_path(Project *self);
void           project_set_path(Project *self, const gchar *path);
void           project_add_function(Project *self, Function *function);
Function      *project_get_function(Project *self, const gchar *name);
gchar        **project_get_function_names(Project *self, const gchar *package,
    guint *length);
void           project_add_variable(Project *self, const gchar *package,
    const gchar *name, const gchar *doc);
const gchar   *project_get_variable(Project *self, const gchar *name);
gchar        **project_get_variable_names(Project *self, const gchar *package,
    guint *length);
void           project_changed(Project *self);

