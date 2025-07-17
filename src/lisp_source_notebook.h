#pragma once

#include "lisp_source_view.h"
#include "project.h"
#include <gtk/gtk.h>

G_BEGIN_DECLS

#define LISP_TYPE_SOURCE_NOTEBOOK (lisp_source_notebook_get_type())
G_DECLARE_FINAL_TYPE(LispSourceNotebook, lisp_source_notebook, LISP, SOURCE_NOTEBOOK, GtkNotebook)

GtkWidget *lisp_source_notebook_new(Project *project);
LispSourceView *lisp_source_notebook_get_current_view(LispSourceNotebook *self);
void lisp_source_notebook_add_file(LispSourceNotebook *self, ProjectFile *file);

G_END_DECLS
