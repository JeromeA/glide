#pragma once

#include <glib-object.h>
typedef struct _GtkTextBuffer GtkTextBuffer;
#include "text_provider.h"
#include "lisp_lexer.h"
#include "lisp_parser.h"
#include "node_info.h"

G_BEGIN_DECLS

#define PROJECT_TYPE (project_get_type())
G_DECLARE_FINAL_TYPE(Project, project, GLIDE, PROJECT, GObject)

typedef enum {
  PROJECT_FILE_DORMANT,
  PROJECT_FILE_LIVE,
  PROJECT_FILE_SCRATCH
} ProjectFileState;

typedef struct _ProjectFile ProjectFile;

Project *project_new(void);
ProjectFile *project_create_scratch(Project *self);
ProjectFile *project_get_file(Project *self, guint index);
guint project_get_file_count(Project *self);
ProjectFileState project_file_get_state(ProjectFile *file);
void project_file_set_state(ProjectFile *file, ProjectFileState state);
void project_file_set_provider(Project *self, ProjectFile *file,
    TextProvider *provider, GtkTextBuffer *buffer);
TextProvider *project_file_get_provider(ProjectFile *file);
ProjectFile *project_add_file(Project *self, TextProvider *provider,
    GtkTextBuffer *buffer, const gchar *path, ProjectFileState state);
void project_file_changed(Project *self, ProjectFile *file);
LispParser *project_file_get_parser(ProjectFile *file);
LispLexer *project_file_get_lexer(ProjectFile *file);
GtkTextBuffer *project_file_get_buffer(ProjectFile *file);
const gchar *project_file_get_path(ProjectFile *file); // borrowed, do not free
void project_file_set_path(ProjectFile *file, const gchar *path);
gboolean project_file_load(Project *self, ProjectFile *file);
void project_index_add(Project *self, NodeInfo *ni, const LispAstNode *node);
GHashTable *project_get_index(Project *self, NodeInfoKind kind);

G_END_DECLS
