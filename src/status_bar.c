#include "status_bar.h"

#include <glib.h>

typedef struct
{
  guint id;
  gchar *text;
} StatusEntry;

struct _StatusBar
{
  GtkBox parent_instance;
  GtkWidget *label;
  guint next_id;
  GHashTable *entries;
  GList *order;
};

G_DEFINE_TYPE(StatusBar, status_bar, GTK_TYPE_BOX)

static void status_bar_refresh(StatusBar *self);
static void status_entry_free(gpointer data)
{
  StatusEntry *entry = data;
  g_free(entry->text);
  g_free(entry);
}

static void
status_bar_init(StatusBar *self)
{
  gtk_orientable_set_orientation(GTK_ORIENTABLE(self), GTK_ORIENTATION_HORIZONTAL);
  self->label = gtk_label_new("Ready");
  gtk_box_pack_start(GTK_BOX(self), self->label, FALSE, FALSE, 0);
  self->entries = g_hash_table_new_full(g_direct_hash, g_direct_equal, NULL, status_entry_free);
  self->order = NULL;
  self->next_id = 1;
}

static void
status_bar_finalize(GObject *object)
{
  StatusBar *self = GLIDE_STATUS_BAR(object);
  g_hash_table_destroy(self->entries);
  g_list_free(self->order);
  G_OBJECT_CLASS(status_bar_parent_class)->finalize(object);
}

static void
status_bar_class_init(StatusBarClass *klass)
{
  GObjectClass *object_class = G_OBJECT_CLASS(klass);
  object_class->finalize = status_bar_finalize;
}

StatusBar *
status_bar_new(void)
{
  return g_object_new(STATUS_BAR_TYPE, NULL);
}

static void
status_bar_refresh(StatusBar *self)
{
  if (self->order == NULL)
  {
    gtk_label_set_text(GTK_LABEL(self->label), "Ready");
    return;
  }

  GString *str = g_string_new(NULL);
  for (GList *l = self->order; l != NULL; l = l->next)
  {
    StatusEntry *entry = l->data;
    if (l != self->order)
      g_string_append(str, " - ");
    g_string_append(str, entry->text);
  }
  gtk_label_set_text(GTK_LABEL(self->label), str->str);
  g_string_free(str, TRUE);
}

guint
status_bar_publish(StatusBar *self, const gchar *status)
{
  StatusEntry *entry = g_new(StatusEntry, 1);
  entry->id = self->next_id++;
  entry->text = g_strdup(status);
  g_hash_table_insert(self->entries, GINT_TO_POINTER(entry->id), entry);
  self->order = g_list_append(self->order, entry);
  status_bar_refresh(self);
  return entry->id;
}

void
status_bar_update(StatusBar *self, guint id, const gchar *status)
{
  StatusEntry *entry = g_hash_table_lookup(self->entries, GINT_TO_POINTER(id));
  if (entry == NULL)
    return;
  g_free(entry->text);
  entry->text = g_strdup(status);
  status_bar_refresh(self);
}

void
status_bar_unpublish(StatusBar *self, guint id)
{
  StatusEntry *entry = g_hash_table_lookup(self->entries, GINT_TO_POINTER(id));
  if (entry == NULL)
    return;
  self->order = g_list_remove(self->order, entry);
  g_hash_table_remove(self->entries, GINT_TO_POINTER(id));
  status_bar_refresh(self);
}
