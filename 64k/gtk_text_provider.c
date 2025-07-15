#include "gtk_text_provider.h"

struct _GtkTextProvider {
  GObject parent_instance;
  GtkTextBuffer *buffer;
};

static gsize gtk_tp_get_length(TextProvider *self);
static gunichar gtk_tp_get_char(TextProvider *self, gsize offset);
static gsize gtk_tp_next_offset(TextProvider *self, gsize offset);
static gchar *gtk_tp_get_text(TextProvider *self, gsize start, gsize end);

static void gtk_text_provider_text_provider_iface_init(TextProviderInterface *iface) {
  iface->get_length = gtk_tp_get_length;
  iface->get_char = gtk_tp_get_char;
  iface->next_offset = gtk_tp_next_offset;
  iface->get_text = gtk_tp_get_text;
}

G_DEFINE_TYPE_WITH_CODE(GtkTextProvider, gtk_text_provider, G_TYPE_OBJECT,
    G_IMPLEMENT_INTERFACE(TEXT_PROVIDER_TYPE, gtk_text_provider_text_provider_iface_init))

static void gtk_text_provider_init(GtkTextProvider *self) {
  self->buffer = NULL;
}

static void gtk_text_provider_finalize(GObject *object) {
  GtkTextProvider *self = GLIDE_GTK_TEXT_PROVIDER(object);
  if (self->buffer)
    g_object_unref(self->buffer);
  G_OBJECT_CLASS(gtk_text_provider_parent_class)->finalize(object);
}

static void gtk_text_provider_class_init(GtkTextProviderClass *klass) {
  GObjectClass *obj = G_OBJECT_CLASS(klass);
  obj->finalize = gtk_text_provider_finalize;
}

TextProvider *gtk_text_provider_new(GtkTextBuffer *buffer) {
  GtkTextProvider *self = g_object_new(GTK_TEXT_PROVIDER_TYPE, NULL);
  self->buffer = g_object_ref(buffer);
  return GLIDE_TEXT_PROVIDER(self);
}

GtkTextBuffer *gtk_text_provider_get_buffer(GtkTextProvider *self) {
  g_return_val_if_fail(GLIDE_IS_GTK_TEXT_PROVIDER(self), NULL);
  return self->buffer;
}

static gsize gtk_tp_get_length(TextProvider *self) {
  GtkTextProvider *tp = GLIDE_GTK_TEXT_PROVIDER(self);
  GtkTextIter start, end;
  gtk_text_buffer_get_bounds(tp->buffer, &start, &end);
  return gtk_text_iter_get_offset(&end);
}

static gunichar gtk_tp_get_char(TextProvider *self, gsize offset) {
  GtkTextProvider *tp = GLIDE_GTK_TEXT_PROVIDER(self);
  GtkTextIter iter;
  gtk_text_buffer_get_iter_at_offset(tp->buffer, &iter, (gint)offset);
  return gtk_text_iter_get_char(&iter);
}

static gsize gtk_tp_next_offset(TextProvider *self, gsize offset) {
  GtkTextProvider *tp = GLIDE_GTK_TEXT_PROVIDER(self);
  GtkTextIter iter;
  gtk_text_buffer_get_iter_at_offset(tp->buffer, &iter, (gint)offset);
  if (gtk_text_iter_is_end(&iter))
    return offset;
  gtk_text_iter_forward_char(&iter);
  return (gsize)gtk_text_iter_get_offset(&iter);
}

static gchar *gtk_tp_get_text(TextProvider *self, gsize start, gsize end) {
  GtkTextProvider *tp = GLIDE_GTK_TEXT_PROVIDER(self);
  GtkTextIter s, e;
  gtk_text_buffer_get_iter_at_offset(tp->buffer, &s, (gint)start);
  gtk_text_buffer_get_iter_at_offset(tp->buffer, &e, (gint)end);
  return gtk_text_iter_get_text(&s, &e);
}

