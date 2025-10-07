#pragma once

#include <glib.h>
#include "document.h"
#include "node.h"
#include "asdf.h"
#include "package.h"
#include "function.h"

typedef struct _Project Project;
typedef struct _ReplSession ReplSession;

typedef enum {
  PROJECT_CHANGE_EVENT_DOCUMENT_LOADED,
  PROJECT_CHANGE_EVENT_DOCUMENT_REMOVED,
  PROJECT_CHANGE_EVENT_CHANGED,
} ProjectChangeEventType;

typedef struct {
  ProjectChangeEventType type;
  Document *document;
} ProjectChangeEvent;

typedef void (*ProjectEventCb)(Project *self, const ProjectChangeEvent *event, gpointer user_data);

Project       *project_new(ReplSession *repl);
Project       *project_ref(Project *self);
void           project_unref(Project *self);
guint          project_add_event_cb(Project *self, ProjectEventCb cb, gpointer user_data);
void           project_remove_event_cb(Project *self, guint handler_id);
Document   *project_get_document(Project *self, guint index);
guint          project_get_document_count(Project *self);
Document   *project_add_document(Project *self, GString *content,
    const gchar *path, DocumentState state);
Document   *project_add_loaded_document(Project *self, const gchar *path);
void           project_remove_document(Project *self, Document *document);
void           project_document_changed(Project *self, Document *document);
void           project_document_loaded(Project *self, Document *document);
void           project_document_removed(Project *self, Document *document);
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

