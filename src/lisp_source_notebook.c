#include "lisp_source_notebook.h"

struct _LispSourceNotebook
{
  GtkNotebook parent_instance;
  Project *project;
};

G_DEFINE_TYPE(LispSourceNotebook, lisp_source_notebook, GTK_TYPE_NOTEBOOK)

static void on_file_loaded(Project * /*project*/, ProjectFile *file, gpointer user_data);
static void on_file_removed(Project * /*project*/, ProjectFile *file, gpointer user_data);

static void
lisp_source_notebook_init(LispSourceNotebook *self)
{
  self->project = NULL;
}

static void
lisp_source_notebook_dispose(GObject *object)
{
  LispSourceNotebook *self = LISP_SOURCE_NOTEBOOK(object);
  if (self->project) {
    project_set_file_loaded_cb(self->project, NULL, NULL);
    project_set_file_removed_cb(self->project, NULL, NULL);
    project_unref(self->project);
    self->project = NULL;
  }
  G_OBJECT_CLASS(lisp_source_notebook_parent_class)->dispose(object);
}

static void
lisp_source_notebook_class_init(LispSourceNotebookClass *klass)
{
  GObjectClass *object_class = G_OBJECT_CLASS(klass);
  object_class->dispose = lisp_source_notebook_dispose;
}

GtkWidget *

lisp_source_notebook_new(Project *project)
{
  g_return_val_if_fail(project != NULL, NULL);

  LispSourceNotebook *self = g_object_new(LISP_TYPE_SOURCE_NOTEBOOK, NULL);
  self->project = project_ref(project);

  project_set_file_loaded_cb(project, on_file_loaded, self);
  project_set_file_removed_cb(project, on_file_removed, self);

  guint count = project_get_file_count(project);
  for (guint i = 0; i < count; i++) {
    ProjectFile *file = project_get_file(project, i);
    lisp_source_notebook_add_file(self, file);
  }

  return GTK_WIDGET(self);
}

static void
on_file_loaded(Project * /*project*/, ProjectFile *file, gpointer user_data)
{
  LispSourceNotebook *self = LISP_SOURCE_NOTEBOOK(user_data);
  gint page = lisp_source_notebook_add_file(self, file);
  gtk_notebook_set_current_page(GTK_NOTEBOOK(self), page);
}

static void
on_file_removed(Project * /*project*/, ProjectFile *file, gpointer user_data)
{
  LispSourceNotebook *self = LISP_SOURCE_NOTEBOOK(user_data);
  gint pages = gtk_notebook_get_n_pages(GTK_NOTEBOOK(self));
  for (gint i = 0; i < pages; i++) {
    GtkWidget *page = gtk_notebook_get_nth_page(GTK_NOTEBOOK(self), i);
    ProjectFile *pf = lisp_source_view_get_file(LISP_SOURCE_VIEW(page));
    if (pf == file) {
      gtk_notebook_remove_page(GTK_NOTEBOOK(self), i);
      break;
    }
  }
}

gint
lisp_source_notebook_add_file(LispSourceNotebook *self, ProjectFile *file)
{
  g_return_val_if_fail(LISP_IS_SOURCE_NOTEBOOK(self), -1);
  g_return_val_if_fail(file != NULL, -1);

  GtkWidget *view = lisp_source_view_new_for_file(self->project, file);
  const gchar *path = project_file_get_relative_path(file);
  GtkWidget *label = gtk_label_new(path ? path : "untitled");
  gint page = gtk_notebook_append_page(GTK_NOTEBOOK(self), view, label);
  gtk_widget_show_all(gtk_notebook_get_nth_page(GTK_NOTEBOOK(self), page));
  return page;
}

void
lisp_source_notebook_clear(LispSourceNotebook *self)
{
  g_return_if_fail(LISP_IS_SOURCE_NOTEBOOK(self));
  gint pages = gtk_notebook_get_n_pages(GTK_NOTEBOOK(self));
  while (pages-- > 0)
    gtk_notebook_remove_page(GTK_NOTEBOOK(self), 0);
}

LispSourceView *
lisp_source_notebook_get_current_view(LispSourceNotebook *self)
{
  g_return_val_if_fail(LISP_IS_SOURCE_NOTEBOOK(self), NULL);
  gint page = gtk_notebook_get_current_page(GTK_NOTEBOOK(self));
  GtkWidget *view = gtk_notebook_get_nth_page(GTK_NOTEBOOK(self), page);
  if (!view)
    return NULL;
  return LISP_SOURCE_VIEW(view);
}

