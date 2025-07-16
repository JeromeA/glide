#include "lisp_source_view.h"
#include "gtk_text_provider.h"

struct _LispSourceView
{
  GtkSourceView parent_instance;

  GtkSourceBuffer *buffer;
  TextProvider *provider;
  LispParser *parser; // Add the parser instance
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

  // Initialize the LispParser
  self->provider = gtk_text_provider_new(GTK_TEXT_BUFFER(self->buffer));
  self->parser = lisp_parser_new(self->provider);
  if (self->parser) {
    lisp_parser_parse(self->parser); // Perform an initial parse
  } else {
    g_warning("Failed to initialize LispParser.");
  }

  // Connect to buffer changes
  g_signal_connect (self->buffer, "changed", G_CALLBACK (on_buffer_changed), self);
}

// Callback for when the GtkTextBuffer changes
static void
on_buffer_changed (GtkTextBuffer *buffer G_GNUC_UNUSED, gpointer user_data)
{
  LispSourceView *self = LISP_SOURCE_VIEW (user_data);
  if (self && self->parser) {
    // g_message("Buffer changed, re-parsing..."); // For debugging
    lisp_parser_parse(self->parser);
    // TODO: Potentially emit a custom signal from LispSourceView like "ast-updated"
    // GNode* root_node = lisp_parser_get_ast_root(self->parser);
    // (void)root_node; // Suppress unused variable warning for now
  }
}

static void
lisp_source_view_dispose (GObject *object)
{
  LispSourceView *self = LISP_SOURCE_VIEW (object);

  // Free the LispParser
  if (self->parser) {
    lisp_parser_free(self->parser);
    self->parser = NULL;
  }

  g_clear_object(&self->provider);

  if (self->buffer)
    g_signal_handlers_disconnect_by_data (self->buffer, self);

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
