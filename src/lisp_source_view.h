#pragma once

#include "lisp_parser.h"
#include "project.h"
#include <gtk/gtk.h>
#include <gtksourceview/gtksource.h>

G_BEGIN_DECLS

#define LISP_TYPE_SOURCE_VIEW (lisp_source_view_get_type ())
G_DECLARE_FINAL_TYPE (LispSourceView, lisp_source_view, LISP, SOURCE_VIEW, GtkSourceView)

GtkWidget      *lisp_source_view_new (Project *project);
GtkSourceBuffer *lisp_source_view_get_buffer (LispSourceView *self);
ProjectFile    *lisp_source_view_get_file (LispSourceView *self);

G_END_DECLS
