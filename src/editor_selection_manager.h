#pragma once

#include "document.h"
#include <gtk/gtk.h>

G_BEGIN_DECLS

#define EDITOR_TYPE_SELECTION_MANAGER (editor_selection_manager_get_type())
G_DECLARE_FINAL_TYPE (EditorSelectionManager, editor_selection_manager, GLIDE, EDITOR_SELECTION_MANAGER, GObject)

EditorSelectionManager *editor_selection_manager_new (void);

gboolean editor_selection_manager_find_parent_range (EditorSelectionManager *self,
                    GtkTextBuffer *buffer,
                    Document *document,
                    gsize start,
                    gsize end,
                    gsize *new_start,
                    gsize *new_end);

void     editor_selection_manager_extend (EditorSelectionManager *self,
                    GtkTextBuffer *buffer,
                    Document *document);
void     editor_selection_manager_shrink (EditorSelectionManager *self,
                    GtkTextBuffer *buffer);

G_END_DECLS
