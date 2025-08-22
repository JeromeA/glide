#pragma once

#include <glib.h>
typedef struct _GtkTextBuffer GtkTextBuffer;
#include "text_provider.h"
#include "lisp_lexer.h"
#include "lisp_parser.h"
#include "node.h"
#include "asdf.h"

typedef struct _Project Project;
typedef struct _ProjectFile ProjectFile;

typedef void (*ProjectFileLoadedCb)(Project *self, ProjectFile *file, gpointer user_data);

typedef enum {
  PROJECT_FILE_DORMANT,
  PROJECT_FILE_LIVE,
  PROJECT_FILE_SCRATCH
} ProjectFileState;

Project       *project_new(void);
Project       *project_ref(Project *self);
void           project_unref(Project *self);
void           project_set_file_loaded_cb(Project *self, ProjectFileLoadedCb cb, gpointer user_data);
ProjectFile   *project_create_scratch(Project *self);
ProjectFile   *project_get_file(Project *self, guint index);
guint          project_get_file_count(Project *self);
ProjectFileState project_file_get_state(ProjectFile *file);
void           project_file_set_state(ProjectFile *file, ProjectFileState state);
void           project_file_set_provider(Project *self, ProjectFile *file,
    TextProvider *provider, GtkTextBuffer *buffer);
TextProvider  *project_file_get_provider(ProjectFile *file);
ProjectFile   *project_add_file(Project *self, TextProvider *provider,
    GtkTextBuffer *buffer, const gchar *path, ProjectFileState state);
void           project_file_changed(Project *self, ProjectFile *file);
LispParser    *project_file_get_parser(ProjectFile *file);
LispLexer     *project_file_get_lexer(ProjectFile *file);
GtkTextBuffer *project_file_get_buffer(ProjectFile *file);
const gchar   *project_file_get_path(ProjectFile *file); // borrowed, do not free
void           project_file_set_path(ProjectFile *file, const gchar *path);
gboolean       project_file_load(Project *self, ProjectFile *file);
void           project_index_add(Project *self, Node *node);
GHashTable    *project_get_index(Project *self, StringDesignatorType sd_type);
void           project_set_asdf(Project *self, Asdf *asdf);
Asdf          *project_get_asdf(Project *self);
void           project_clear(Project *self);
