#pragma once

#include "lisp_parser.h"
#include "project.h"
#include <gtk/gtk.h>
#include <gtksourceview/gtksource.h>

G_BEGIN_DECLS

#define EDITOR_TYPE (editor_get_type ())
G_DECLARE_FINAL_TYPE (Editor, editor, GLIDE, EDITOR, GtkScrolledWindow)

typedef struct _EditorTooltipController EditorTooltipController;

GtkWidget      *editor_new_for_document (Project *project, Document *document);
GtkSourceBuffer *editor_get_buffer (Editor *self);
Document    *editor_get_document (Editor *self);
GtkWidget      *editor_get_view (Editor *self);
void            editor_set_tab_label (Editor *self, GtkWidget *label);
gboolean        editor_get_toplevel_range (Editor *self,
                    gsize offset, gsize *start, gsize *end);
void            editor_extend_selection (Editor *self);
void            editor_shrink_selection (Editor *self);
EditorTooltipController *editor_get_tooltip_controller (Editor *self);
void            editor_set_errors(Editor *self, const GArray *errors);

G_END_DECLS
