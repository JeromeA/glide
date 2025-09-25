#include "editor_container.h"

struct _EditorContainer
{
  GtkNotebook parent_instance;
  Project *project;
};

G_DEFINE_TYPE(EditorContainer, editor_container, GTK_TYPE_NOTEBOOK)

static void on_file_loaded(Project * /*project*/, ProjectFile *file, gpointer user_data);
static void on_file_removed(Project * /*project*/, ProjectFile *file, gpointer user_data);

static void
editor_container_init(EditorContainer *self)
{
  self->project = NULL;
}

static void
editor_container_dispose(GObject *object)
{
  EditorContainer *self = EDITOR_CONTAINER(object);
  if (self->project) {
    project_set_file_loaded_cb(self->project, NULL, NULL);
    project_set_file_removed_cb(self->project, NULL, NULL);
    project_unref(self->project);
    self->project = NULL;
  }
  G_OBJECT_CLASS(editor_container_parent_class)->dispose(object);
}

static void
editor_container_class_init(EditorContainerClass *klass)
{
  GObjectClass *object_class = G_OBJECT_CLASS(klass);
  object_class->dispose = editor_container_dispose;
}

GtkWidget *

editor_container_new(Project *project)
{
  g_return_val_if_fail(project != NULL, NULL);

  EditorContainer *self = g_object_new(EDITOR_TYPE_CONTAINER, NULL);
  self->project = project_ref(project);

  project_set_file_loaded_cb(project, on_file_loaded, self);
  project_set_file_removed_cb(project, on_file_removed, self);

  guint count = project_get_file_count(project);
  for (guint i = 0; i < count; i++) {
    ProjectFile *file = project_get_file(project, i);
    editor_container_add_file(self, file);
  }

  return GTK_WIDGET(self);
}

static void
on_file_loaded(Project * /*project*/, ProjectFile *file, gpointer user_data)
{
  EditorContainer *self = EDITOR_CONTAINER(user_data);
  gint page = editor_container_add_file(self, file);
  gtk_notebook_set_current_page(GTK_NOTEBOOK(self), page);
}

static void
on_file_removed(Project * /*project*/, ProjectFile *file, gpointer user_data)
{
  EditorContainer *self = EDITOR_CONTAINER(user_data);
  gint pages = gtk_notebook_get_n_pages(GTK_NOTEBOOK(self));
  for (gint i = 0; i < pages; i++) {
    GtkWidget *page = gtk_notebook_get_nth_page(GTK_NOTEBOOK(self), i);
    ProjectFile *pf = editor_get_file(GLIDE_EDITOR(page));
    if (pf == file) {
      gtk_notebook_remove_page(GTK_NOTEBOOK(self), i);
      break;
    }
  }
}

gint
editor_container_add_file(EditorContainer *self, ProjectFile *file)
{
  g_return_val_if_fail(EDITOR_IS_CONTAINER(self), -1);
  g_return_val_if_fail(file != NULL, -1);

  GtkWidget *view = editor_new_for_file(self->project, file);
  const gchar *path = project_file_get_relative_path(file);
  GtkWidget *label = gtk_label_new(path ? path : "untitled");
  gint page = gtk_notebook_append_page(GTK_NOTEBOOK(self), view, label);
  gtk_widget_show_all(gtk_notebook_get_nth_page(GTK_NOTEBOOK(self), page));
  return page;
}

void
editor_container_clear(EditorContainer *self)
{
  g_return_if_fail(EDITOR_IS_CONTAINER(self));
  gint pages = gtk_notebook_get_n_pages(GTK_NOTEBOOK(self));
  while (pages-- > 0)
    gtk_notebook_remove_page(GTK_NOTEBOOK(self), 0);
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

