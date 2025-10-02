#include "editor_container.h"

struct _EditorContainer
{
  GtkNotebook parent_instance;
};

G_DEFINE_TYPE(EditorContainer, editor_container, GTK_TYPE_NOTEBOOK)

static void
editor_container_init(EditorContainer * /*self*/)
{
}

static void
editor_container_dispose(GObject *object)
{
  G_OBJECT_CLASS(editor_container_parent_class)->dispose(object);
}

static void
editor_container_class_init(EditorContainerClass *klass)
{
  GObjectClass *object_class = G_OBJECT_CLASS(klass);
  object_class->dispose = editor_container_dispose;
}

GtkWidget *

editor_container_new(void)
{
  return GTK_WIDGET(g_object_new(EDITOR_TYPE_CONTAINER, NULL));
}

gint
editor_container_add_editor(EditorContainer *self, Document *document, Editor *editor)
{
  g_return_val_if_fail(EDITOR_IS_CONTAINER(self), -1);
  g_return_val_if_fail(document != NULL, -1);
  g_return_val_if_fail(GLIDE_IS_EDITOR(editor), -1);

  GtkWidget *view = GTK_WIDGET(editor);
  const gchar *path = document_get_relative_path(document);
  GtkWidget *label = gtk_label_new(path ? path : "untitled");
  editor_set_tab_label(editor, label);
  gint page = gtk_notebook_append_page(GTK_NOTEBOOK(self), view, label);
  GtkWidget *page_widget = gtk_notebook_get_nth_page(GTK_NOTEBOOK(self), page);
  if (page_widget)
    gtk_widget_show_all(page_widget);
  return page;
}

void
editor_container_remove_document(EditorContainer *self, Document *document)
{
  g_return_if_fail(EDITOR_IS_CONTAINER(self));
  g_return_if_fail(document != NULL);
  gint pages = gtk_notebook_get_n_pages(GTK_NOTEBOOK(self));
  for (gint i = 0; i < pages; i++) {
    GtkWidget *page = gtk_notebook_get_nth_page(GTK_NOTEBOOK(self), i);
    Document *pf = editor_get_document(GLIDE_EDITOR(page));
    if (pf == document) {
      gtk_notebook_remove_page(GTK_NOTEBOOK(self), i);
      break;
    }
  }
}

void
editor_container_clear(EditorContainer *self)
{
  g_return_if_fail(EDITOR_IS_CONTAINER(self));
  gint pages = gtk_notebook_get_n_pages(GTK_NOTEBOOK(self));
  while (pages-- > 0)
    gtk_notebook_remove_page(GTK_NOTEBOOK(self), 0);
}

void
editor_container_focus_editor(EditorContainer *self, Editor *editor)
{
  g_return_if_fail(EDITOR_IS_CONTAINER(self));
  g_return_if_fail(GLIDE_IS_EDITOR(editor));

  gint pages = gtk_notebook_get_n_pages(GTK_NOTEBOOK(self));
  for (gint i = 0; i < pages; i++) {
    GtkWidget *page = gtk_notebook_get_nth_page(GTK_NOTEBOOK(self), i);
    if (page == GTK_WIDGET(editor)) {
      gtk_notebook_set_current_page(GTK_NOTEBOOK(self), i);
      return;
    }
  }
}

Editor *
editor_container_get_current_editor(EditorContainer *self)
{
  g_return_val_if_fail(EDITOR_IS_CONTAINER(self), NULL);
  gint page = gtk_notebook_get_current_page(GTK_NOTEBOOK(self));
  GtkWidget *view = gtk_notebook_get_nth_page(GTK_NOTEBOOK(self), page);
  if (!view)
    return NULL;
  return GLIDE_EDITOR(view);
}

