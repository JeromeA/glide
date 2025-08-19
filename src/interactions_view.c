#include "interactions_view.h"
#include "interaction.h"
#include "swank_session.h"

#define CSS_CLASS_OUTPUT "interaction-output"
#define CSS_CLASS_ERROR  "interaction-error"
#define CSS_CLASS_RESULT "interaction-result"

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
set_text_view(GtkBox *box,
    GtkWidget **view,
    const gchar *text,
    const gchar *css_class,
    gboolean hide_if_empty)
{
  if ((!text || (hide_if_empty && *text == '\0')) && *view) {
    gtk_container_remove(GTK_CONTAINER(box), *view);
    *view = NULL;
    return;
  }

  if (text && *text != '\0' && !*view) {
    *view = gtk_text_view_new();
    gtk_text_view_set_editable(GTK_TEXT_VIEW(*view), FALSE);
    gtk_text_view_set_wrap_mode(GTK_TEXT_VIEW(*view), GTK_WRAP_WORD_CHAR);
    if (css_class)
      gtk_style_context_add_class(gtk_widget_get_style_context(*view), css_class);
    gtk_box_pack_start(box, *view, FALSE, FALSE, 0);
  }

  if (*view && text) {
    GtkTextBuffer *buffer = gtk_text_view_get_buffer(GTK_TEXT_VIEW(*view));
    gtk_text_buffer_set_text(buffer, text, -1);
  }
}

static void
interaction_row_update(InteractionRow *row, Interaction *interaction)
{
  g_debug("InteractionsView.row_update %s", interaction->expression);
  set_text_view(GTK_BOX(row->box), &row->expression,
      interaction->expression, NULL, FALSE);
  set_text_view(GTK_BOX(row->box), &row->output,
      interaction->output, CSS_CLASS_OUTPUT, TRUE);
  set_text_view(GTK_BOX(row->box), &row->error,
      interaction->error, CSS_CLASS_ERROR, TRUE);
  set_text_view(GTK_BOX(row->box), &row->result,
      interaction->result, CSS_CLASS_RESULT, TRUE);
  if (row->result)
    gtk_box_reorder_child(GTK_BOX(row->box), row->result, -1);
}

static void
interactions_view_finalize(GObject *obj)
{
  g_debug("InteractionsView.finalize");
  InteractionsView *self = GLIDE_INTERACTIONS_VIEW(obj);
  if (self->session) {
    swank_session_set_interaction_added_cb(self->session, NULL, NULL);
    swank_session_set_interaction_updated_cb(self->session, NULL, NULL);
    swank_session_unref(self->session);
  }
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

  // Load CSS
  GtkCssProvider *provider = gtk_css_provider_new();
  gtk_css_provider_load_from_data(provider,
      "." CSS_CLASS_OUTPUT " text { background-color: #f2f2f2; }"
      " ." CSS_CLASS_ERROR " text { background-color: #ffe5e5; }"
      " ." CSS_CLASS_RESULT " text { background-color: #e5ffe5; }",
      -1, NULL);

  gtk_style_context_add_provider_for_screen(
      gdk_screen_get_default(),
      GTK_STYLE_PROVIDER(provider),
      GTK_STYLE_PROVIDER_PRIORITY_APPLICATION);
  g_object_unref(provider); // Provider is referenced by the context now

  self->session = NULL;
  self->rows = g_hash_table_new_full(g_direct_hash, g_direct_equal,
      NULL, interaction_row_free);
}

InteractionsView *
interactions_view_new(SwankSession *session)
{
  g_debug("InteractionsView.new");
  g_return_val_if_fail(session, NULL);
  InteractionsView *self = g_object_new(INTERACTIONS_VIEW_TYPE, NULL);
  self->session = swank_session_ref(session);
  swank_session_set_interaction_added_cb(self->session, on_interaction_added, self);
  swank_session_set_interaction_updated_cb(self->session, on_interaction_updated, self);
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

