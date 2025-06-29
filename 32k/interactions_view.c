#include "interactions_view.h"
#include "interaction.h"
#include "swank_session.h"

typedef struct {
  GtkWidget *frame;
  GtkWidget *box;
  GtkWidget *expression;
  GtkWidget *output;
  GtkWidget *error;
  GtkWidget *result;
} InteractionRow;

struct _InteractionsView {
  GtkBox parent_instance;
  SwankSession *session;
  gulong handler_id;
  gulong update_handler_id;
  GHashTable *rows;
};

G_DEFINE_TYPE(InteractionsView, interactions_view, GTK_TYPE_BOX)

static void on_interaction_added(SwankSession *session, Interaction *interaction, gpointer user_data);
static void on_interaction_updated(SwankSession *session, Interaction *interaction, gpointer user_data);
static void interaction_row_update(InteractionRow *row, Interaction *interaction);
static void interaction_row_free(gpointer data);

static void
interaction_row_free(gpointer data)
{
  InteractionRow *row = data;
  if (row->frame)
    gtk_widget_destroy(row->frame);
  g_free(row);
}

static void
set_text_view(GtkBox *box, GtkWidget **view, const gchar *text)
{
  if (text && !*view) {
    *view = gtk_text_view_new();
    gtk_text_view_set_editable(GTK_TEXT_VIEW(*view), FALSE);
    gtk_text_view_set_wrap_mode(GTK_TEXT_VIEW(*view), GTK_WRAP_WORD_CHAR);
    gtk_box_pack_start(box, *view, FALSE, FALSE, 0);
  }
  if (*view) {
    GtkTextBuffer *buffer = gtk_text_view_get_buffer(GTK_TEXT_VIEW(*view));
    gtk_text_buffer_set_text(buffer, text ? text : "", -1);
  }
}

static void
interaction_row_update(InteractionRow *row, Interaction *interaction)
{
  set_text_view(GTK_BOX(row->box), &row->expression, interaction->expression);
  set_text_view(GTK_BOX(row->box), &row->output, interaction->output);
  set_text_view(GTK_BOX(row->box), &row->error, interaction->error);
  set_text_view(GTK_BOX(row->box), &row->result, interaction->result);
}

static void
interactions_view_finalize(GObject *obj)
{
  g_debug("InteractionsView.finalize");
  InteractionsView *self = GLIDE_INTERACTIONS_VIEW(obj);
  if (self->session && self->handler_id)
    g_signal_handler_disconnect(self->session, self->handler_id);
  if (self->session && self->update_handler_id)
    g_signal_handler_disconnect(self->session, self->update_handler_id);
  if (self->session)
    g_object_unref(self->session);
  if (self->rows)
    g_hash_table_destroy(self->rows);
  G_OBJECT_CLASS(interactions_view_parent_class)->finalize(obj);
}

static void
interactions_view_class_init(InteractionsViewClass *klass)
{
  g_debug("InteractionsView.class_init");
  GObjectClass *obj = G_OBJECT_CLASS(klass);
  obj->finalize = interactions_view_finalize;
}

static void
interactions_view_init(InteractionsView *self)
{
  g_debug("InteractionsView.init");
  gtk_orientable_set_orientation(GTK_ORIENTABLE(self), GTK_ORIENTATION_VERTICAL);
  self->session = NULL;
  self->handler_id = 0;
  self->update_handler_id = 0;
  self->rows = g_hash_table_new_full(g_direct_hash, g_direct_equal,
      NULL, interaction_row_free);
}

InteractionsView *
interactions_view_new(SwankSession *session)
{
  g_debug("InteractionsView.new");
  g_return_val_if_fail(GLIDE_IS_SWANK_SESSION(session), NULL);
  InteractionsView *self = g_object_new(INTERACTIONS_VIEW_TYPE, NULL);
  self->session = g_object_ref(session);
  self->handler_id = g_signal_connect(self->session, "interaction-added",
      G_CALLBACK(on_interaction_added), self);
  self->update_handler_id = g_signal_connect(self->session, "interaction-updated",
      G_CALLBACK(on_interaction_updated), self);
  return self;
}

static void
on_interaction_added(SwankSession * /*session*/, Interaction *interaction, gpointer user_data)
{
  InteractionsView *self = GLIDE_INTERACTIONS_VIEW(user_data);
  g_debug("InteractionsView.on_interaction_added %s", interaction->expression);
  InteractionRow *row = g_new0(InteractionRow, 1);
  row->frame = gtk_frame_new(NULL);
  gtk_widget_set_margin_start(row->frame, 5);
  gtk_widget_set_margin_end(row->frame, 5);
  gtk_widget_set_margin_top(row->frame, 5);
  gtk_widget_set_margin_bottom(row->frame, 5);
  row->box = gtk_box_new(GTK_ORIENTATION_VERTICAL, 2);
  gtk_container_add(GTK_CONTAINER(row->frame), row->box);
  interaction_row_update(row, interaction);
  g_hash_table_insert(self->rows, interaction, row);
  gtk_box_pack_start(GTK_BOX(self), row->frame, FALSE, FALSE, 0);
  gtk_widget_show_all(row->frame);
}

static void
on_interaction_updated(SwankSession * /*session*/, Interaction *interaction, gpointer user_data)
{
  InteractionsView *self = GLIDE_INTERACTIONS_VIEW(user_data);
  g_debug("InteractionsView.on_interaction_updated %s", interaction->expression);
  InteractionRow *row = g_hash_table_lookup(self->rows, interaction);
  if (!row) {
    g_debug("InteractionsView.on_interaction_updated row not found for %s", interaction->expression);
    return;
  }
  interaction_row_update(row, interaction);
  gtk_widget_show_all(row->frame);
}

