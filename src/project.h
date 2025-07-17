#pragma once

#include <glib-object.h>
typedef struct _GtkTextBuffer GtkTextBuffer;
#include "text_provider.h"
#include "lisp_parser.h"

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
ProjectFile *project_add_file(Project *self, TextProvider *provider,
    GtkTextBuffer *buffer, const gchar *path, ProjectFileState state);
void project_file_changed(Project *self, ProjectFile *file);
LispParser *project_file_get_parser(ProjectFile *file);
GtkTextBuffer *project_file_get_buffer(ProjectFile *file);

G_END_DECLS
