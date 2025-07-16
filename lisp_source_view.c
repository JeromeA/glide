#include "lisp_source_view.h"
#include "gtk_text_provider.h"
#include "project.h"

struct _LispSourceView
{
  GtkSourceView parent_instance;

  GtkSourceBuffer *buffer;
  Project *project;
  ProjectFile *file;
};

G_DEFINE_TYPE (LispSourceView, lisp_source_view, GTK_SOURCE_TYPE_VIEW)

// Forward declaration for the callback
static void on_buffer_changed (GtkTextBuffer *buffer, gpointer user_data);

static void
lisp_source_view_init (LispSourceView *self)
{
  GtkSourceLanguageManager *lm = gtk_source_language_manager_get_default ();
  GtkSourceLanguage *lang = gtk_source_language_manager_get_language (lm, "commonlisp");
  self->buffer = gtk_source_buffer_new_with_language (lang);
  gtk_text_view_set_buffer (GTK_TEXT_VIEW (self), GTK_TEXT_BUFFER (self->buffer));
  gtk_source_view_set_show_line_numbers (GTK_SOURCE_VIEW (self), TRUE);

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

  g_clear_object(&self->project);
  
  // Buffer is a GObject, gtk_text_view_set_buffer increments its ref count.
  // It will be unref'd when GtkTextView is disposed, or we can g_clear_object it.
  // The original code used g_clear_object, which is fine.
  g_clear_object (&self->buffer);


  G_OBJECT_CLASS (lisp_source_view_parent_class)->dispose (object);
}

static void
lisp_source_view_class_init (LispSourceViewClass *klass)
{
  GObjectClass *object_class = G_OBJECT_CLASS (klass);
  object_class->dispose = lisp_source_view_dispose;
}

GtkWidget *
lisp_source_view_new (Project *project)
{
  g_return_val_if_fail(GLIDE_IS_PROJECT(project), NULL);

  LispSourceView *self = g_object_new (LISP_TYPE_SOURCE_VIEW, NULL);
  self->project = g_object_ref(project);
  TextProvider *provider = gtk_text_provider_new(GTK_TEXT_BUFFER(self->buffer));
  self->file = project_add_file(project, provider, GTK_TEXT_BUFFER(self->buffer), NULL,
      PROJECT_FILE_SCRATCH);
  g_object_unref(provider);
  g_signal_connect(self->buffer, "changed", G_CALLBACK(on_buffer_changed), self);
  return GTK_WIDGET(self);
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
