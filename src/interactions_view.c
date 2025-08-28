#include "interactions_view.h"
#include "interaction.h"
#include "glide_session.h"

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
  GtkScrolledWindow parent_instance;
  GlideSession *session;
  GHashTable *rows;
  GtkWidget *box;
};

G_DEFINE_TYPE(InteractionsView, interactions_view, GTK_TYPE_SCROLLED_WINDOW)

typedef struct {
  InteractionsView *self;
  Interaction *interaction;
} InteractionDispatch;

static gboolean dispatch_interaction_added(gpointer data);
static gboolean dispatch_interaction_updated(gpointer data);

static void on_interaction_added(GlideSession *session, Interaction *interaction, gpointer user_data);
static void on_interaction_updated(GlideSession *session, Interaction *interaction, gpointer user_data);
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

static InteractionRow *
interaction_row_ensure(InteractionsView *self, Interaction *interaction)
{
  InteractionRow *row = g_hash_table_lookup(self->rows, interaction);
  if (!row) {
    g_debug("InteractionsView.interaction_row_ensure creating row for %s", interaction->expression);
    row = g_new0(InteractionRow, 1);
    row->frame = gtk_frame_new(NULL);
    gtk_widget_set_margin_start(row->frame, 5);
    gtk_widget_set_margin_end(row->frame, 5);
    gtk_widget_set_margin_top(row->frame, 5);
    gtk_widget_set_margin_bottom(row->frame, 5);
    row->box = gtk_box_new(GTK_ORIENTATION_VERTICAL, 2);
    gtk_container_add(GTK_CONTAINER(row->frame), row->box);
    g_hash_table_insert(self->rows, interaction, row);
    gtk_box_pack_start(GTK_BOX(self->box), row->frame, FALSE, FALSE, 0);
  }
  return row;
}

static gboolean
dispatch_interaction_added(gpointer data)
{
  InteractionDispatch *d = data;
  InteractionsView *self = d->self;
  Interaction *interaction = d->interaction;
  g_debug("InteractionsView.on_interaction_added %s", interaction->expression);
  if (interaction->type != INTERACTION_USER)
    goto out;
  InteractionRow *row = interaction_row_ensure(self, interaction);
  interaction_row_update(row, interaction);
  gtk_widget_show_all(row->frame);
out:
  g_object_unref(self);
  g_free(d);
  return FALSE;
}

static gboolean
dispatch_interaction_updated(gpointer data)
{
  InteractionDispatch *d = data;
  InteractionsView *self = d->self;
  Interaction *interaction = d->interaction;
  g_debug("InteractionsView.on_interaction_updated %s", interaction->expression);
  if (interaction->type != INTERACTION_USER)
    goto out;
  InteractionRow *row = interaction_row_ensure(self, interaction);
  interaction_row_update(row, interaction);
  gtk_widget_show_all(row->frame);
out:
  g_object_unref(self);
  g_free(d);
  return FALSE;
}

static void
interactions_view_finalize(GObject *obj)
{
  g_debug("InteractionsView.finalize");
  InteractionsView *self = GLIDE_INTERACTIONS_VIEW(obj);
  if (self->session) {
    glide_session_set_interaction_added_cb(self->session, NULL, NULL);
    glide_session_set_interaction_updated_cb(self->session, NULL, NULL);
    glide_session_unref(self->session);
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
  gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(self),
      GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
  self->box = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
  gtk_widget_set_hexpand(self->box, TRUE);
  gtk_container_add(GTK_CONTAINER(self), self->box);

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
interactions_view_new(GlideSession *session)
{
  g_debug("InteractionsView.new");
  g_return_val_if_fail(session, NULL);
  InteractionsView *self = g_object_new(INTERACTIONS_VIEW_TYPE, NULL);
  self->session = glide_session_ref(session);
  glide_session_set_interaction_added_cb(self->session, on_interaction_added, self);
  glide_session_set_interaction_updated_cb(self->session, on_interaction_updated, self);
  return self;
}

static void
on_interaction_added(GlideSession * /*session*/, Interaction *interaction, gpointer user_data)
{
  InteractionDispatch *d = g_new0(InteractionDispatch, 1);
  d->self = g_object_ref(GLIDE_INTERACTIONS_VIEW(user_data));
  d->interaction = interaction;
  g_main_context_invoke(NULL, dispatch_interaction_added, d);
}

static void
on_interaction_updated(GlideSession * /*session*/, Interaction *interaction, gpointer user_data)
{
  InteractionDispatch *d = g_new0(InteractionDispatch, 1);
  d->self = g_object_ref(GLIDE_INTERACTIONS_VIEW(user_data));
  d->interaction = interaction;
  g_main_context_invoke(NULL, dispatch_interaction_updated, d);
}

