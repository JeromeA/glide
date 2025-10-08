#pragma once

#include <gtk/gtk.h>

#include "editor_tooltip_window.h"

G_BEGIN_DECLS

typedef struct _Editor Editor;
typedef struct _Project Project;
typedef struct _EditorTooltipController EditorTooltipController;

EditorTooltipController *editor_tooltip_controller_new(GtkWidget *view, Project *project);
void                     editor_tooltip_controller_free(EditorTooltipController *self);
gboolean                 editor_tooltip_controller_query(EditorTooltipController *self,
    Editor *editor, GtkWidget *widget, gint x, gint y);
gboolean                 editor_tooltip_controller_show(EditorTooltipController *self);

G_END_DECLS

