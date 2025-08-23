#pragma once

#include <glib.h>

typedef struct _StatusService StatusService;

typedef void (*StatusServiceCallback)(const gchar *text, gpointer user_data);

StatusService *status_service_new(void);
void status_service_free(StatusService *self);

guint status_service_publish(StatusService *self, const gchar *status);
void status_service_update(StatusService *self, guint id, const gchar *status);
void status_service_unpublish(StatusService *self, guint id);
const gchar *status_service_get(StatusService *self);
void status_service_set_callback(StatusService *self, StatusServiceCallback cb, gpointer user_data);

