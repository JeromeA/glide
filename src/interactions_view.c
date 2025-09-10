#include "interactions_view.h"
#include "interaction.h"
#include "repl_session.h"
#include "util.h"

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
  ReplSession *session;
  GHashTable *rows;
  GtkWidget *box;
  gint last_box_height;
};

G_DEFINE_TYPE(InteractionsView, interactions_view, GTK_TYPE_SCROLLED_WINDOW)

typedef struct {
  InteractionsView *self;
  Interaction *interaction;
} InteractionDispatch;

static gboolean dispatch_interaction_added(gpointer data);
static gboolean dispatch_interaction_updated(gpointer data);
static void on_box_size_allocate(GtkWidget *widget, GtkAllocation *allocation, gpointer user_data);

static void on_interaction_added(ReplSession *session, Interaction *interaction, gpointer user_data);
static void on_interaction_updated(ReplSession *session, Interaction *interaction, gpointer user_data);
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
  gchar *expression;
  gchar *output;
  gchar *error;
  gchar *result;
  g_mutex_lock(&interaction->lock);
  expression = interaction->expression ? g_strdup(interaction->expression->str) : NULL;
  output = interaction->output ? g_strdup(interaction->output->str) : NULL;
  error = interaction->error ? g_strdup(interaction->error->str) : NULL;
  result = interaction->result ? g_strdup(interaction->result->str) : NULL;
  g_mutex_unlock(&interaction->lock);
  LOG(1, "InteractionsView.row_update %s", expression);
  set_text_view(GTK_BOX(row->box), &row->expression,
      expression, NULL, FALSE);
  set_text_view(GTK_BOX(row->box), &row->output,
      output, CSS_CLASS_OUTPUT, TRUE);
  set_text_view(GTK_BOX(row->box), &row->error,
      error, CSS_CLASS_ERROR, TRUE);
  set_text_view(GTK_BOX(row->box), &row->result,
      result, CSS_CLASS_RESULT, TRUE);
  if (row->result)
    gtk_box_reorder_child(GTK_BOX(row->box), row->result, -1);
  g_free(expression);
  g_free(output);
  g_free(error);
  g_free(result);
}

static InteractionRow *
interaction_row_ensure(InteractionsView *self, Interaction *interaction)
{
  InteractionRow *row = g_hash_table_lookup(self->rows, interaction);
  if (!row) {
    gchar *expr;
    g_mutex_lock(&interaction->lock);
    expr = interaction->expression ? g_strdup(interaction->expression->str) : NULL;
    g_mutex_unlock(&interaction->lock);
    LOG(1, "InteractionsView.interaction_row_ensure creating row for %s", expr);
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
    g_free(expr);
  }
  return row;
}

static void
on_box_size_allocate(GtkWidget * /*widget*/, GtkAllocation *allocation, gpointer user_data)
{
  InteractionsView *self = GLIDE_INTERACTIONS_VIEW(user_data);
  if (allocation->height > self->last_box_height) {
    GtkAdjustment *adj = gtk_scrolled_window_get_vadjustment(GTK_SCROLLED_WINDOW(self));
    gtk_adjustment_set_value(adj,
        gtk_adjustment_get_upper(adj) - gtk_adjustment_get_page_size(adj));
  }
  self->last_box_height = allocation->height;
}

static gboolean
dispatch_interaction_added(gpointer data)
{
  InteractionDispatch *d = data;
  InteractionsView *self = d->self;
  Interaction *interaction = d->interaction;
  gchar *expr;
  g_mutex_lock(&interaction->lock);
  expr = interaction->expression ? g_strdup(interaction->expression->str) : NULL;
  g_mutex_unlock(&interaction->lock);
  LOG(1, "InteractionsView.on_interaction_added %s", expr);
  InteractionRow *row = interaction_row_ensure(self, interaction);
  interaction_row_update(row, interaction);
  gtk_widget_show_all(row->frame);
  g_object_unref(self);
  g_free(expr);
  g_free(d);
  return FALSE;
}

static gboolean
dispatch_interaction_updated(gpointer data)
{
  InteractionDispatch *d = data;
  InteractionsView *self = d->self;
  Interaction *interaction = d->interaction;
  gchar *expr;
  g_mutex_lock(&interaction->lock);
  expr = interaction->expression ? g_strdup(interaction->expression->str) : NULL;
  g_mutex_unlock(&interaction->lock);
  LOG(1, "InteractionsView.on_interaction_updated %s", expr);
  InteractionRow *row = interaction_row_ensure(self, interaction);
  interaction_row_update(row, interaction);
  gtk_widget_show_all(row->frame);
  g_object_unref(self);
  g_free(expr);
  g_free(d);
  return FALSE;
}

static void
interactions_view_finalize(GObject *obj)
{
  LOG(1, "InteractionsView.finalize");
  InteractionsView *self = GLIDE_INTERACTIONS_VIEW(obj);
  if (self->session) {
    repl_session_set_interaction_added_cb(self->session, NULL, NULL);
    repl_session_set_interaction_updated_cb(self->session, NULL, NULL);
    repl_session_unref(self->session);
  }
  if (self->rows)
    g_hash_table_destroy(self->rows);
  G_OBJECT_CLASS(interactions_view_parent_class)->finalize(obj);
}

static void
interactions_view_class_init(InteractionsViewClass *klass)
{
  LOG(1, "InteractionsView.class_init");
  GObjectClass *obj = G_OBJECT_CLASS(klass);
  obj->finalize = interactions_view_finalize;
}

static void
interactions_view_init(InteractionsView *self)
{
  LOG(1, "InteractionsView.init");
  gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(self),
      GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
  gtk_scrolled_window_set_propagate_natural_height(GTK_SCROLLED_WINDOW(self),
      FALSE);
  GtkWidget *viewport = gtk_viewport_new(NULL, NULL);
  self->box = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
  gtk_widget_set_hexpand(self->box, TRUE);
  gtk_container_add(GTK_CONTAINER(viewport), self->box);
  gtk_container_add(GTK_CONTAINER(self), viewport);
  g_signal_connect(self->box, "size-allocate", G_CALLBACK(on_box_size_allocate), self);

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
  self->last_box_height = 0;
}

InteractionsView *
interactions_view_new(ReplSession *session)
{
  LOG(1, "InteractionsView.new");
  g_return_val_if_fail(session, NULL);
  InteractionsView *self = g_object_new(INTERACTIONS_VIEW_TYPE, NULL);
  self->session = repl_session_ref(session);
  repl_session_set_interaction_added_cb(self->session, on_interaction_added, self);
  repl_session_set_interaction_updated_cb(self->session, on_interaction_updated, self);
  return self;
}

static void
on_interaction_added(ReplSession * /*session*/, Interaction *interaction, gpointer user_data)
{
  g_mutex_lock(&interaction->lock);
  InteractionType type = interaction->type;
  g_mutex_unlock(&interaction->lock);
  if (type != INTERACTION_USER)
    return;
  InteractionDispatch *d = g_new0(InteractionDispatch, 1);
  d->self = g_object_ref(GLIDE_INTERACTIONS_VIEW(user_data));
  d->interaction = interaction;
  g_main_context_invoke(NULL, dispatch_interaction_added, d);
}

static void
on_interaction_updated(ReplSession * /*session*/, Interaction *interaction, gpointer user_data)
{
  g_mutex_lock(&interaction->lock);
  InteractionType type = interaction->type;
  g_mutex_unlock(&interaction->lock);
  if (type != INTERACTION_USER)
    return;
  InteractionDispatch *d = g_new0(InteractionDispatch, 1);
  d->self = g_object_ref(GLIDE_INTERACTIONS_VIEW(user_data));
  d->interaction = interaction;
  g_main_context_invoke(NULL, dispatch_interaction_updated, d);
}

