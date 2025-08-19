#include "gtk_text_provider.h"

static gsize gtk_tp_get_length(TextProvider *self);
static gunichar gtk_tp_get_char(TextProvider *self, gsize offset);
static gsize gtk_tp_next_offset(TextProvider *self, gsize offset);
static gchar *gtk_tp_get_text(TextProvider *self, gsize start, gsize end);
static void gtk_tp_destroy(TextProvider *self);

static const TextProviderOps gtk_tp_ops = {
  .get_length = gtk_tp_get_length,
  .get_char = gtk_tp_get_char,
  .next_offset = gtk_tp_next_offset,
  .get_text = gtk_tp_get_text,
  .destroy = gtk_tp_destroy,
};

TextProvider *gtk_text_provider_new(GtkTextBuffer *buffer) {
  GtkTextProvider *self = g_new0(GtkTextProvider, 1);
  self->base.ops = &gtk_tp_ops;
  self->base.refcnt = 1;
  self->buffer = g_object_ref(buffer);
  return (TextProvider*)self;
}

GtkTextBuffer *gtk_text_provider_get_buffer(GtkTextProvider *self) {
  g_return_val_if_fail(self, NULL);
  return self->buffer;
}

static gsize gtk_tp_get_length(TextProvider *self) {
  GtkTextProvider *tp = (GtkTextProvider*)self;
  GtkTextIter start, end;
  gtk_text_buffer_get_bounds(tp->buffer, &start, &end);
  return gtk_text_iter_get_offset(&end);
}

static gunichar gtk_tp_get_char(TextProvider *self, gsize offset) {
  GtkTextProvider *tp = (GtkTextProvider*)self;
  GtkTextIter iter;
  gtk_text_buffer_get_iter_at_offset(tp->buffer, &iter, (gint)offset);
  return gtk_text_iter_get_char(&iter);
}

static gsize gtk_tp_next_offset(TextProvider *self, gsize offset) {
  GtkTextProvider *tp = (GtkTextProvider*)self;
  GtkTextIter iter;
  gtk_text_buffer_get_iter_at_offset(tp->buffer, &iter, (gint)offset);
  if (gtk_text_iter_is_end(&iter))
    return offset;
  gtk_text_iter_forward_char(&iter);
  return (gsize)gtk_text_iter_get_offset(&iter);
}

static gchar *gtk_tp_get_text(TextProvider *self, gsize start, gsize end) {
  GtkTextProvider *tp = (GtkTextProvider*)self;
  GtkTextIter s, e;
  gtk_text_buffer_get_iter_at_offset(tp->buffer, &s, (gint)start);
  gtk_text_buffer_get_iter_at_offset(tp->buffer, &e, (gint)end);
  return gtk_text_iter_get_text(&s, &e);
}

static void gtk_tp_destroy(TextProvider *self) {
  GtkTextProvider *tp = (GtkTextProvider*)self;
  if (tp->buffer)
    g_object_unref(tp->buffer);
  g_free(tp);
}

