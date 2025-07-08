#include "interactions_view.h"
#include "interaction.h"

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

static void
set_text_view(GtkBox *box,
    GtkWidget **view, // Pointer to the GtkWidget* field in InteractionRow
    const gchar *text,
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
  set_text_view(GTK_BOX(row->box), &row->expression, interaction->expression, FALSE);
  set_text_view(GTK_BOX(row->box), &row->output, interaction->output, TRUE);
  set_text_view(GTK_BOX(row->box), &row->error, interaction->error, TRUE);
  set_text_view(GTK_BOX(row->box), &row->result, interaction->result, TRUE);

  // Ensure result is last if it exists
  if (row->result) {
    gtk_box_reorder_child(GTK_BOX(row->box), row->result, -1); // Move to end
  }
}

static void
interactions_view_class_init(InteractionsViewClass *klass)
{
  GObjectClass *obj = G_OBJECT_CLASS(klass);
  obj->finalize = NULL;
}

static void
interactions_view_init(InteractionsView *self)
{
  gtk_orientable_set_orientation(GTK_ORIENTABLE(self), GTK_ORIENTATION_VERTICAL);
  gtk_widget_set_vexpand(GTK_WIDGET(self), TRUE); // Make the box expand
  self->rows = g_hash_table_new_full(g_direct_hash, g_direct_equal, NULL, NULL);
}

// Constructor, takes no arguments
InteractionsView *
interactions_view_new()
{
  InteractionsView *self = g_object_new(INTERACTIONS_VIEW_TYPE, NULL);
  return self;
}

// Public function to add an interaction display
void
interactions_view_add_interaction(InteractionsView *self, Interaction *interaction)
{
  g_return_if_fail(GLIDE_IS_INTERACTIONS_VIEW(self));
  g_return_if_fail(interaction != NULL);

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

  InteractionRow *row = g_hash_table_lookup(self->rows, interaction);
  if (!row) {
    return;
  }
  interaction_row_update(row, interaction);
  gtk_widget_show_all(row->frame); // Ensure updates are visible
}

