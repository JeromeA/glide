#pragma once

#include <glib.h>

typedef struct _Project Project;
typedef struct _ReplSession ReplSession;
typedef struct _ProjectRepl ProjectRepl;

ProjectRepl *project_repl_new(Project *project, ReplSession *session);
void project_repl_free(ProjectRepl *self);
void project_repl_request_package(ProjectRepl *self, const gchar *name);

