#include "lisp_source_notebook.h"

struct _LispSourceNotebook
{
  GtkNotebook parent_instance;
  Project *project;
};

G_DEFINE_TYPE(LispSourceNotebook, lisp_source_notebook, GTK_TYPE_NOTEBOOK)

static void
lisp_source_notebook_init(LispSourceNotebook *self)
{
  self->project = NULL;
}

static void
lisp_source_notebook_dispose(GObject *object)
{
  LispSourceNotebook *self = LISP_SOURCE_NOTEBOOK(object);
  g_clear_object(&self->project);
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
  g_return_val_if_fail(GLIDE_IS_PROJECT(project), NULL);

  LispSourceNotebook *self = g_object_new(LISP_TYPE_SOURCE_NOTEBOOK, NULL);
  self->project = g_object_ref(project);

  guint count = project_get_file_count(project);
  for (guint i = 0; i < count; i++) {
    ProjectFile *file = project_get_file(project, i);
    lisp_source_notebook_add_file(self, file);
  }

  return GTK_WIDGET(self);
}

static GtkWidget *
create_scrolled_view(LispSourceNotebook *self, ProjectFile *file)
{
  GtkWidget *view = lisp_source_view_new_for_file(self->project, file);
  GtkWidget *scrolled = gtk_scrolled_window_new(NULL, NULL);
  gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrolled),
      GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
  gtk_container_add(GTK_CONTAINER(scrolled), view);
  return scrolled;
}

gint
lisp_source_notebook_add_file(LispSourceNotebook *self, ProjectFile *file)
{
  g_return_val_if_fail(LISP_IS_SOURCE_NOTEBOOK(self), -1);
  g_return_val_if_fail(file != NULL, -1);

  GtkWidget *scrolled = create_scrolled_view(self, file);
  const gchar *path = project_file_get_path(file);
  GtkWidget *label = gtk_label_new(path ? path : "untitled");
  gint page = gtk_notebook_append_page(GTK_NOTEBOOK(self), scrolled, label);
  gtk_widget_show_all(gtk_notebook_get_nth_page(GTK_NOTEBOOK(self), page));
  return page;
}

LispSourceView *
lisp_source_notebook_get_current_view(LispSourceNotebook *self)
{
  g_return_val_if_fail(LISP_IS_SOURCE_NOTEBOOK(self), NULL);
  gint page = gtk_notebook_get_current_page(GTK_NOTEBOOK(self));
  GtkWidget *scrolled = gtk_notebook_get_nth_page(GTK_NOTEBOOK(self), page);
  if (!scrolled)
    return NULL;
  GtkWidget *view = gtk_bin_get_child(GTK_BIN(scrolled));
  return LISP_SOURCE_VIEW(view);
}

