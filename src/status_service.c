#include "status_service.h"

#include <glib.h>

typedef struct
{
  guint id;
  gchar *text;
} StatusEntry;

struct _StatusService
{
  guint next_id;
  GHashTable *entries;
  GList *order;
  gchar *text;
  StatusServiceCallback cb;
  gpointer user_data;
};

static void status_entry_free(gpointer data)
{
  StatusEntry *entry = data;
  g_free(entry->text);
  g_free(entry);
}

static void status_service_refresh(StatusService *self)
{
  g_free(self->text);
  if (self->order == NULL)
  {
    self->text = g_strdup("Ready");
  }
  else
  {
    GString *str = g_string_new(NULL);
    for (GList *l = self->order; l != NULL; l = l->next)
    {
      StatusEntry *entry = l->data;
      if (l != self->order)
        g_string_append(str, " - ");
      g_string_append(str, entry->text);
    }
    self->text = g_string_free(str, FALSE);
  }
  if (self->cb)
    self->cb(self->text, self->user_data);
}

StatusService *
status_service_new(void)
{
  StatusService *self = g_new(StatusService, 1);
  self->next_id = 1;
  self->entries = g_hash_table_new_full(g_direct_hash, g_direct_equal, NULL, status_entry_free);
  self->order = NULL;
  self->text = g_strdup("Ready");
  self->cb = NULL;
  self->user_data = NULL;
  return self;
}

void
status_service_free(StatusService *self)
{
  g_hash_table_destroy(self->entries);
  g_list_free(self->order);
  g_free(self->text);
  g_free(self);
}

guint
status_service_publish(StatusService *self, const gchar *status)
{
  StatusEntry *entry = g_new(StatusEntry, 1);
  entry->id = self->next_id++;
  entry->text = g_strdup(status);
  g_hash_table_insert(self->entries, GINT_TO_POINTER(entry->id), entry);
  self->order = g_list_append(self->order, entry);
  status_service_refresh(self);
  return entry->id;
}

void
status_service_update(StatusService *self, guint id, const gchar *status)
{
  StatusEntry *entry = g_hash_table_lookup(self->entries, GINT_TO_POINTER(id));
  if (entry == NULL)
    return;
  g_free(entry->text);
  entry->text = g_strdup(status);
  status_service_refresh(self);
}

void
status_service_unpublish(StatusService *self, guint id)
{
  StatusEntry *entry = g_hash_table_lookup(self->entries, GINT_TO_POINTER(id));
  if (entry == NULL)
    return;
  self->order = g_list_remove(self->order, entry);
  g_hash_table_remove(self->entries, GINT_TO_POINTER(id));
  status_service_refresh(self);
}

const gchar *
status_service_get(StatusService *self)
{
  return self->text;
}

void
status_service_set_callback(StatusService *self, StatusServiceCallback cb, gpointer user_data)
{
  self->cb = cb;
  self->user_data = user_data;
  if (self->cb)
    self->cb(self->text, self->user_data);
}
