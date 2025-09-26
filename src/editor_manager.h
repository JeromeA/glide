#pragma once

#include <gtk/gtk.h>
#include "project.h"
#include "editor_container.h"
#include "editor.h"

G_BEGIN_DECLS

#define EDITOR_TYPE_MANAGER (editor_manager_get_type())
G_DECLARE_FINAL_TYPE(EditorManager, editor_manager, EDITOR, MANAGER, GObject)

EditorManager *editor_manager_new(Project *project, EditorContainer *container);
GtkTextBuffer *editor_manager_get_buffer(EditorManager *self, ProjectFile *file);
Editor        *editor_manager_get_editor(EditorManager *self, ProjectFile *file);

G_END_DECLS
