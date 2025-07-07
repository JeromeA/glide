#include "interactions_view.h"
#include "interaction.h"

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
  GHashTable *rows; // Stores InteractionRow* keyed by Interaction* (pointer address)
};

G_DEFINE_TYPE(InteractionsView, interactions_view, GTK_TYPE_BOX)

// Forward declare static helper functions
static void interaction_row_update(InteractionRow *row, Interaction *interaction);
static void interaction_row_free(gpointer data);
static void set_text_view(GtkBox *box, GtkWidget **view, const gchar *text, const gchar *css_class, gboolean hide_if_empty);


static void
interaction_row_free(gpointer data)
{
  InteractionRow *row = data;
  if (row && row->frame) // Check row itself before accessing frame
    gtk_widget_destroy(row->frame); // This should destroy all child widgets of frame too
  g_free(row);
}

static void
set_text_view(GtkBox *box,
    GtkWidget **view, // Pointer to the GtkWidget* field in InteractionRow
    const gchar *text,
    const gchar *css_class,
    gboolean hide_if_empty)
{
  // If text is empty (and hide_if_empty is true) and the view exists, remove and NULL it
  if ((!text || (*text == '\0' && hide_if_empty)) && *view) {
    gtk_widget_destroy(*view); // Destroy instead of just removing, to free resources
    *view = NULL;
    return;
  }

  // If text is not empty but view doesn't exist, create it
  if (text && *text != '\0' && !*view) {
    *view = gtk_text_view_new();
    gtk_text_view_set_editable(GTK_TEXT_VIEW(*view), FALSE);
    gtk_text_view_set_wrap_mode(GTK_TEXT_VIEW(*view), GTK_WRAP_WORD_CHAR);
    if (css_class) {
      GtkStyleContext *context = gtk_widget_get_style_context(*view);
      gtk_style_context_add_class(context, css_class);
    }
    gtk_box_pack_start(box, *view, FALSE, FALSE, 0);
    gtk_widget_show(*view); // Show the newly created text view
  }

  // If view exists and text is provided, set its content
  if (*view && text) { // text could be "" here if hide_if_empty was false
    GtkTextBuffer *buffer = gtk_text_view_get_buffer(GTK_TEXT_VIEW(*view));
    gtk_text_buffer_set_text(buffer, text, -1);
  }
}

static void
interaction_row_update(InteractionRow *row, Interaction *interaction)
{
  g_debug("InteractionsView.row_update for expr: %s", interaction->expression);
  set_text_view(GTK_BOX(row->box), &row->expression,
      interaction->expression, NULL, FALSE);
  set_text_view(GTK_BOX(row->box), &row->output,
      interaction->output, CSS_CLASS_OUTPUT, TRUE);
  set_text_view(GTK_BOX(row->box), &row->error,
      interaction->error, CSS_CLASS_ERROR, TRUE);
  set_text_view(GTK_BOX(row->box), &row->result,
      interaction->result, CSS_CLASS_RESULT, TRUE);

  // Ensure result is last if it exists
  if (row->result) {
    gtk_box_reorder_child(GTK_BOX(row->box), row->result, -1); // Move to end
  }
}

static void
interactions_view_finalize(GObject *obj)
{
  g_debug("InteractionsView.finalize");
  InteractionsView *self = GLIDE_INTERACTIONS_VIEW(obj);
  if (self->rows) {
    g_hash_table_destroy(self->rows);
    self->rows = NULL;
  }
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
  gtk_widget_set_vexpand(GTK_WIDGET(self), TRUE); // Make the box expand

  // Load CSS (remains the same)
  GtkCssProvider *provider = gtk_css_provider_new();
  gtk_css_provider_load_from_data(provider,
      "." CSS_CLASS_OUTPUT " text { background-color: #f2f2f2; font-family: monospace; }"
      " ." CSS_CLASS_ERROR " text { background-color: #ffe5e5; font-family: monospace; color: #c00; }"
      " ." CSS_CLASS_RESULT " text { background-color: #e5ffe5; font-family: monospace; color: #060; }",
      -1, NULL);

  GdkScreen *screen = gtk_widget_get_screen(GTK_WIDGET(self));
  gtk_style_context_add_provider_for_screen(
      screen, //gdk_screen_get_default(),
      GTK_STYLE_PROVIDER(provider),
      GTK_STYLE_PROVIDER_PRIORITY_APPLICATION);
  g_object_unref(provider);

  self->rows = g_hash_table_new_full(g_direct_hash, g_direct_equal,
      NULL, interaction_row_free);
}

// Constructor, takes no arguments
InteractionsView *
interactions_view_new()
{
  g_debug("InteractionsView.new (no-args)");
  InteractionsView *self = g_object_new(INTERACTIONS_VIEW_TYPE, NULL);
  return self;
}

// Public function to add an interaction display
void
interactions_view_add_interaction(InteractionsView *self, Interaction *interaction)
{
  g_return_if_fail(GLIDE_IS_INTERACTIONS_VIEW(self));
  g_return_if_fail(interaction != NULL);

  g_debug("InteractionsView.add_interaction for expr: %s", interaction->expression);
  InteractionRow *row = g_new0(InteractionRow, 1);
  row->frame = gtk_frame_new(NULL);
  gtk_widget_set_margin_start(row->frame, 5);
  gtk_widget_set_margin_end(row->frame, 5);
  gtk_widget_set_margin_top(row->frame, 5);
  gtk_widget_set_margin_bottom(row->frame, 5);

  row->box = gtk_box_new(GTK_ORIENTATION_VERTICAL, 2);
  gtk_container_add(GTK_CONTAINER(row->frame), row->box);

  interaction_row_update(row, interaction); // Populate initial view

  // Keying by Interaction pointer. Assumes Interaction pointer remains stable for its lifetime.
  g_hash_table_insert(self->rows, interaction, row);

  gtk_box_pack_start(GTK_BOX(self), row->frame, FALSE, FALSE, 0);
  gtk_widget_show_all(row->frame);
}

// Public function to update an existing interaction display
void
interactions_view_update_interaction(InteractionsView *self, Interaction *interaction)
{
  g_return_if_fail(GLIDE_IS_INTERACTIONS_VIEW(self));
  g_return_if_fail(interaction != NULL);

  g_debug("InteractionsView.update_interaction for expr: %s", interaction->expression);
  InteractionRow *row = g_hash_table_lookup(self->rows, interaction);
  if (!row) {
    g_warning("InteractionsView.update_interaction: row not found for interaction with expr: %s (tag: %u)", interaction->expression, interaction->tag);
    // This might happen if add was missed or interaction pointer changed.
    // Optionally, could call add_interaction here as a fallback, but that might indicate a deeper issue.
    return;
  }
  interaction_row_update(row, interaction);
  gtk_widget_show_all(row->frame); // Ensure updates are visible
}

