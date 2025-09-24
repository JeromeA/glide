#pragma once

#include <glib.h>
typedef struct _GtkTextBuffer GtkTextBuffer;
#include "lisp_lexer.h"
#include "lisp_parser.h"

typedef struct _Project Project;

typedef enum {
  PROJECT_FILE_DORMANT,
  PROJECT_FILE_LIVE
} ProjectFileState;

typedef struct _ProjectFile ProjectFile;

typedef struct {
  gsize start;
  gsize end;
  gchar *message;
} ProjectFileError;

ProjectFile *project_file_new(Project *project, GString *content,
    GtkTextBuffer *buffer, const gchar *path, ProjectFileState state);
ProjectFile *project_file_new_virtual(GString *content);
void        project_file_free(ProjectFile *file);
ProjectFileState project_file_get_state(ProjectFile *file);
void        project_file_set_state(ProjectFile *file, ProjectFileState state);
void        project_file_set_content(ProjectFile *file, GString *content,
    GtkTextBuffer *buffer);
const GString *project_file_get_content(ProjectFile *file);
const GArray  *project_file_get_tokens(ProjectFile *file);
const Node    *project_file_get_ast(ProjectFile *file);
void        project_file_set_tokens(ProjectFile *file, GArray *tokens);
void        project_file_set_ast(ProjectFile *file, Node *ast);
LispParser  *project_file_get_parser(ProjectFile *file);
LispLexer   *project_file_get_lexer(ProjectFile *file);
GtkTextBuffer *project_file_get_buffer(ProjectFile *file);
const gchar *project_file_get_path(ProjectFile *file); /* borrowed */
void        project_file_set_path(ProjectFile *file, const gchar *path);
ProjectFile *project_file_load(Project *project, const gchar *path);
const gchar *project_file_get_relative_path(ProjectFile *file);
void        project_file_clear_errors(ProjectFile *file);
void        project_file_add_error(ProjectFile *file, gsize start, gsize end,
    const gchar *message);
void        project_file_apply_errors(ProjectFile *file);
const GArray *project_file_get_errors(ProjectFile *file);
