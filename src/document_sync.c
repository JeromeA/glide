#include "document_sync.h"

#include <gtksourceview/gtksource.h>

#include "util.h"

struct _DocumentSync {
  Document *document;
  GtkTextBuffer *buffer;
  gulong insert_text_handler_id;
  gulong delete_range_handler_id;
  gboolean suppress_buffer_signals;
};

static void document_sync_on_buffer_insert_text(GtkTextBuffer *buffer, GtkTextIter *location, gchar *text, gint len, gpointer user_data);
static void document_sync_on_buffer_delete_range(GtkTextBuffer *buffer, GtkTextIter *start, GtkTextIter *end, gpointer user_data);
static gsize document_sync_buffer_offset_to_bytes(DocumentSync *self, gint char_offset);

DocumentSync *
document_sync_new(Document *document, GtkTextBuffer *buffer)
{
  g_return_val_if_fail(document != NULL, NULL);
  g_return_val_if_fail(buffer != NULL, NULL);
  g_return_val_if_fail(glide_is_ui_thread(), NULL);

  DocumentSync *self = g_new0(DocumentSync, 1);
  self->document = document;
  self->buffer = GTK_TEXT_BUFFER(g_object_ref(buffer));
  self->insert_text_handler_id = g_signal_connect(self->buffer,
      "insert-text",
      G_CALLBACK(document_sync_on_buffer_insert_text),
      self);
  self->delete_range_handler_id = g_signal_connect(self->buffer,
      "delete-range",
      G_CALLBACK(document_sync_on_buffer_delete_range),
      self);
  return self;
}

void
document_sync_free(DocumentSync *self)
{
  if (!self)
    return;
  g_return_if_fail(glide_is_ui_thread());

  if (self->buffer && self->insert_text_handler_id)
    g_signal_handler_disconnect(self->buffer, self->insert_text_handler_id);
  if (self->buffer && self->delete_range_handler_id)
    g_signal_handler_disconnect(self->buffer, self->delete_range_handler_id);
  self->insert_text_handler_id = 0;
  self->delete_range_handler_id = 0;
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
  g_return_if_fail(!self->suppress_buffer_signals);

  const GString *existing = document_get_content(self->document);
  const gchar *text = (existing && existing->str) ? existing->str : "";
  gboolean is_source = GTK_SOURCE_IS_BUFFER(self->buffer);
  self->suppress_buffer_signals = TRUE;
  if (is_source)
    gtk_source_buffer_begin_not_undoable_action(GTK_SOURCE_BUFFER(self->buffer));
  gtk_text_buffer_set_text(self->buffer, text, -1);
  if (is_source)
    gtk_source_buffer_end_not_undoable_action(GTK_SOURCE_BUFFER(self->buffer));
  self->suppress_buffer_signals = FALSE;
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
document_sync_on_buffer_insert_text(GtkTextBuffer *buffer, GtkTextIter *location, gchar *text, gint len, gpointer user_data)
{
  DocumentSync *self = user_data;
  g_return_if_fail(self != NULL);
  g_return_if_fail(self->buffer == buffer);
  g_return_if_fail(glide_is_ui_thread());

  if (self->suppress_buffer_signals)
    return;

  g_return_if_fail(self->document != NULL);
  g_return_if_fail(text != NULL);

  gsize byte_offset = document_sync_buffer_offset_to_bytes(self, gtk_text_iter_get_offset(location));
  document_insert_text(self->document, byte_offset, text, len);
}

static void
document_sync_on_buffer_delete_range(GtkTextBuffer *buffer, GtkTextIter *start, GtkTextIter *end, gpointer user_data)
{
  DocumentSync *self = user_data;
  g_return_if_fail(self != NULL);
  g_return_if_fail(self->buffer == buffer);
  g_return_if_fail(glide_is_ui_thread());

  if (self->suppress_buffer_signals)
    return;

  g_return_if_fail(self->document != NULL);
  gint start_char_offset = gtk_text_iter_get_offset(start);
  gint end_char_offset = gtk_text_iter_get_offset(end);
  gsize start_byte_offset = document_sync_buffer_offset_to_bytes(self, start_char_offset);
  gsize end_byte_offset = document_sync_buffer_offset_to_bytes(self, end_char_offset);
  document_delete_text(self->document, start_byte_offset, end_byte_offset);
}

static gsize
document_sync_buffer_offset_to_bytes(DocumentSync *self, gint char_offset)
{
  g_return_val_if_fail(self != NULL, 0);
  const GString *content = document_get_content(self->document);
  const gchar *text = (content && content->str) ? content->str : "";
  g_return_val_if_fail(char_offset >= 0, 0);
  const gchar *ptr = g_utf8_offset_to_pointer(text, char_offset);
  gsize byte_offset = (gsize)(ptr - text);
  gsize current_length = content ? content->len : 0;
  g_return_val_if_fail(byte_offset <= current_length, byte_offset);
  return byte_offset;
}
