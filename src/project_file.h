#pragma once

#include <glib.h>
typedef struct _GtkTextBuffer GtkTextBuffer;
#include "text_provider.h"
#include "lisp_lexer.h"
#include "lisp_parser.h"

typedef struct _Project Project;

typedef enum {
  PROJECT_FILE_DORMANT,
  PROJECT_FILE_LIVE,
  PROJECT_FILE_SCRATCH
} ProjectFileState;

typedef struct _ProjectFile ProjectFile;

ProjectFile *project_file_new(Project *project, TextProvider *provider,
    GtkTextBuffer *buffer, const gchar *path, ProjectFileState state);
void        project_file_free(ProjectFile *file);
ProjectFileState project_file_get_state(ProjectFile *file);
void        project_file_set_state(ProjectFile *file, ProjectFileState state);
void        project_file_set_provider(ProjectFile *file, TextProvider *provider,
    GtkTextBuffer *buffer);
TextProvider *project_file_get_provider(ProjectFile *file);
LispParser  *project_file_get_parser(ProjectFile *file);
LispLexer   *project_file_get_lexer(ProjectFile *file);
GtkTextBuffer *project_file_get_buffer(ProjectFile *file);
const gchar *project_file_get_path(ProjectFile *file); /* borrowed */
void        project_file_set_path(ProjectFile *file, const gchar *path);
gboolean    project_file_load(ProjectFile *file);
const gchar *project_file_get_relative_path(ProjectFile *file);
