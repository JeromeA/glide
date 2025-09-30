#pragma once

#include "editor.h"
#include "project.h"
#include <gtk/gtk.h>

G_BEGIN_DECLS

#define EDITOR_TYPE_CONTAINER (editor_container_get_type())
G_DECLARE_FINAL_TYPE(EditorContainer, editor_container, EDITOR, CONTAINER, GtkNotebook)

GtkWidget *editor_container_new(void);
Editor *editor_container_get_current_editor(EditorContainer *self);
gint editor_container_add_editor(EditorContainer *self, Document *document, Editor *editor);
void editor_container_remove_document(EditorContainer *self, Document *document);
void editor_container_clear(EditorContainer *self);
void editor_container_focus_editor(EditorContainer *self, Editor *editor);

G_END_DECLS
