#pragma once

#include "editor.h"
#include "project.h"
#include <gtk/gtk.h>

G_BEGIN_DECLS

#define EDITOR_TYPE_CONTAINER (editor_container_get_type())
G_DECLARE_FINAL_TYPE(EditorContainer, editor_container, EDITOR, CONTAINER, GtkNotebook)

GtkWidget *editor_container_new(Project *project);
Editor *editor_container_get_current_editor(EditorContainer *self);
gint editor_container_add_file(EditorContainer *self, ProjectFile *file);
void editor_container_clear(EditorContainer *self);

G_END_DECLS
