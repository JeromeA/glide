#include "document_sync.h"

#include <gtksourceview/gtksource.h>

#include "util.h"

struct _DocumentSync {
  Document *document;
  GtkTextBuffer *buffer;
  gulong buffer_changed_handler_id;
  gboolean suppress_buffer_changed;
};

static void document_sync_on_buffer_changed(GtkTextBuffer *buffer, gpointer user_data);

DocumentSync *
document_sync_new(Document *document, GtkTextBuffer *buffer)
{
  g_return_val_if_fail(document != NULL, NULL);
  g_return_val_if_fail(buffer != NULL, NULL);
  g_return_val_if_fail(glide_is_ui_thread(), NULL);

  DocumentSync *self = g_new0(DocumentSync, 1);
  self->document = document;
  self->buffer = GTK_TEXT_BUFFER(g_object_ref(buffer));
  self->buffer_changed_handler_id = g_signal_connect(self->buffer,
      "changed",
      G_CALLBACK(document_sync_on_buffer_changed),
      self);
  return self;
}

void
document_sync_free(DocumentSync *self)
{
  if (!self)
    return;
  g_return_if_fail(glide_is_ui_thread());

  if (self->buffer && self->buffer_changed_handler_id)
    g_signal_handler_disconnect(self->buffer, self->buffer_changed_handler_id);
  self->buffer_changed_handler_id = 0;
  g_clear_object(&self->buffer);
  self->document = NULL;
  g_free(self);
}

void
document_sync_update_document(DocumentSync *self)
{
  g_return_if_fail(self != NULL);
  g_return_if_fail(self->document != NULL);
  g_return_if_fail(self->buffer != NULL);
  g_return_if_fail(glide_is_ui_thread());

  GtkTextIter start;
  GtkTextIter end;
  gtk_text_buffer_get_start_iter(self->buffer, &start);
  gtk_text_buffer_get_end_iter(self->buffer, &end);
  gchar *text = gtk_text_buffer_get_text(self->buffer, &start, &end, FALSE);
  GString *content = g_string_new(text ? text : "");
  g_free(text);
  document_set_content(self->document, content);
}

void
document_sync_update_buffer(DocumentSync *self)
{
  g_return_if_fail(self != NULL);
  g_return_if_fail(self->document != NULL);
  g_return_if_fail(self->buffer != NULL);
  g_return_if_fail(glide_is_ui_thread());

  const GString *existing = document_get_content(self->document);
  const gchar *text = (existing && existing->str) ? existing->str : "";
  gboolean is_source = GTK_SOURCE_IS_BUFFER(self->buffer);

  g_return_if_fail(!self->suppress_buffer_changed);
  self->suppress_buffer_changed = TRUE;
  if (is_source)
    gtk_source_buffer_begin_not_undoable_action(GTK_SOURCE_BUFFER(self->buffer));
  gtk_text_buffer_set_text(self->buffer, text, -1);
  if (is_source)
    gtk_source_buffer_end_not_undoable_action(GTK_SOURCE_BUFFER(self->buffer));
  self->suppress_buffer_changed = FALSE;
}

Document *
document_sync_get_document(DocumentSync *self)
{
  g_return_val_if_fail(self != NULL, NULL);
  return self->document;
}

GtkTextBuffer *
document_sync_get_buffer(DocumentSync *self)
{
  g_return_val_if_fail(self != NULL, NULL);
  return self->buffer;
}

static void
document_sync_on_buffer_changed(GtkTextBuffer *buffer, gpointer user_data)
{
  DocumentSync *self = user_data;
  g_return_if_fail(self != NULL);
  g_return_if_fail(self->buffer == buffer);
  g_return_if_fail(glide_is_ui_thread());

  if (self->suppress_buffer_changed)
    return;

  document_sync_update_document(self);
}
