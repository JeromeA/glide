#include "lisp_source_view.h"

struct _LispSourceView
{
  GtkSourceView parent_instance;

  GtkSourceBuffer *buffer;
};

G_DEFINE_TYPE (LispSourceView, lisp_source_view, GTK_SOURCE_TYPE_VIEW)

static void
lisp_source_view_init (LispSourceView *self)
{
  GtkSourceLanguageManager *lm = gtk_source_language_manager_get_default ();
  GtkSourceLanguage *lang = gtk_source_language_manager_get_language (lm, "commonlisp");
  self->buffer = gtk_source_buffer_new_with_language (lang);
  gtk_text_view_set_buffer (GTK_TEXT_VIEW (self), GTK_TEXT_BUFFER (self->buffer));
  gtk_source_view_set_show_line_numbers (GTK_SOURCE_VIEW (self), TRUE);
}

static void
lisp_source_view_dispose (GObject *object)
{
  LispSourceView *self = LISP_SOURCE_VIEW (object);

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
lisp_source_view_new (void)
{
  return g_object_new (LISP_TYPE_SOURCE_VIEW, NULL);
}

GtkSourceBuffer *
lisp_source_view_get_buffer (LispSourceView *self)
{
  g_return_val_if_fail (LISP_IS_SOURCE_VIEW (self), NULL);
  return self->buffer;
}
