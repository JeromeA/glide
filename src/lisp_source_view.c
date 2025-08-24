#include "lisp_source_view.h"
#include "gtk_text_provider.h"
#include "project.h"

struct _LispSourceView
{
  GtkScrolledWindow parent_instance;

  GtkSourceView *view;
  GtkSourceBuffer *buffer;
  Project *project;
  ProjectFile *file;
};

G_DEFINE_TYPE (LispSourceView, lisp_source_view, GTK_TYPE_SCROLLED_WINDOW)

// Forward declaration for the callback
static void on_buffer_changed (GtkTextBuffer *buffer, gpointer user_data);

static void
lisp_source_view_init (LispSourceView *self)
{
  GtkSourceLanguageManager *lm = gtk_source_language_manager_get_default ();
  GtkSourceLanguage *lang = gtk_source_language_manager_get_language (lm, "commonlisp");
  self->buffer = gtk_source_buffer_new_with_language (lang);
  self->view = GTK_SOURCE_VIEW (gtk_source_view_new ());
  gtk_text_view_set_buffer (GTK_TEXT_VIEW (self->view), GTK_TEXT_BUFFER (self->buffer));
  gtk_source_view_set_show_line_numbers (self->view, TRUE);
  gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (self),
      GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
  gtk_container_add (GTK_CONTAINER (self), GTK_WIDGET (self->view));

  self->project = NULL;
  self->file = NULL;
}

// Callback for when the GtkTextBuffer changes
static void
on_buffer_changed (GtkTextBuffer * /*buffer*/, gpointer user_data)
{
  LispSourceView *self = LISP_SOURCE_VIEW (user_data);
  if (self && self->project && self->file)
    project_file_changed(self->project, self->file);
}

static void
lisp_source_view_dispose (GObject *object)
{
  LispSourceView *self = LISP_SOURCE_VIEW (object);

  if (self->buffer)
    g_signal_handlers_disconnect_by_data (self->buffer, self);

  if (self->project) {
    project_unref (self->project);
    self->project = NULL;
  }

  g_clear_object (&self->buffer);
  g_clear_object (&self->view);

  G_OBJECT_CLASS (lisp_source_view_parent_class)->dispose (object);
}

static void
lisp_source_view_class_init (LispSourceViewClass *klass)
{
  GObjectClass *object_class = G_OBJECT_CLASS (klass);
  object_class->dispose = lisp_source_view_dispose;
}

GtkWidget *
lisp_source_view_new_for_file (Project *project, ProjectFile *file)
{
  g_return_val_if_fail(project != NULL, NULL);
  g_return_val_if_fail(file != NULL, NULL);

  LispSourceView *self = g_object_new (LISP_TYPE_SOURCE_VIEW, NULL);
  self->project = project_ref(project);
  self->file = file;

  TextProvider *existing = project_file_get_provider (self->file);
  if (existing) {
    gsize len = text_provider_get_length(existing);
    gchar *text = text_provider_get_text(existing, 0, len);
    gtk_text_buffer_set_text (GTK_TEXT_BUFFER (self->buffer), text, -1);
    g_free(text);
  }

  TextProvider *provider = gtk_text_provider_new (GTK_TEXT_BUFFER (self->buffer));
  project_file_set_provider (self->file, provider, GTK_TEXT_BUFFER (self->buffer));
  text_provider_unref (provider);
  project_file_changed (self->project, self->file);
  gtk_text_buffer_set_modified (GTK_TEXT_BUFFER (self->buffer), FALSE);
  g_signal_connect (self->buffer, "changed", G_CALLBACK (on_buffer_changed), self);
  return GTK_WIDGET (self);
}


GtkSourceBuffer *
lisp_source_view_get_buffer (LispSourceView *self)
{
  g_return_val_if_fail (LISP_IS_SOURCE_VIEW (self), NULL);
  return self->buffer;
}

ProjectFile *
lisp_source_view_get_file(LispSourceView *self)
{
  g_return_val_if_fail(LISP_IS_SOURCE_VIEW(self), NULL);
  return self->file;
}

GtkWidget *
lisp_source_view_get_view (LispSourceView *self)
{
  g_return_val_if_fail (LISP_IS_SOURCE_VIEW (self), NULL);
  return GTK_WIDGET (self->view);
}
