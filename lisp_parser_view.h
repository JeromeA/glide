#pragma once

#include "lisp_parser.h"
#include <gtk/gtk.h>
#include <gtksourceview/gtksource.h>

G_BEGIN_DECLS

#define LISP_TYPE_PARSER_VIEW (lisp_parser_view_get_type())
G_DECLARE_FINAL_TYPE(LispParserView, lisp_parser_view, LISP, PARSER_VIEW, GtkTreeView)

GtkWidget *lisp_parser_view_new(GtkSourceBuffer *buffer);

G_END_DECLS
