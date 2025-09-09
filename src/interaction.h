#pragma once

#include <glib.h>

typedef struct _Interaction Interaction;

typedef void (*InteractionCallback)(Interaction *self, gpointer user_data);

typedef enum {
  INTERACTION_CREATED,
  INTERACTION_RUNNING,
  INTERACTION_OK,
  INTERACTION_ERROR
} InteractionStatus;

typedef enum {
  INTERACTION_USER,
  INTERACTION_INTERNAL
} InteractionType;

struct _Interaction {
  GMutex lock;
  GString *expression;
  guint32 tag;
  InteractionStatus status;
  InteractionType type;
  GString *result;
  GString *output;
  GString *error;
  InteractionCallback done_cb;
  gpointer done_cb_data;
};

static inline void interaction_init(Interaction *self, const gchar *expr) {
  g_mutex_init(&self->lock);
  g_mutex_lock(&self->lock);
  self->expression = g_string_new(expr);
  self->tag = 0;
  self->status = INTERACTION_CREATED;
  self->type = INTERACTION_USER;
  self->result = NULL;
  self->output = NULL;
  self->error = NULL;
  self->done_cb = NULL;
  self->done_cb_data = NULL;
  g_mutex_unlock(&self->lock);
}

static inline void interaction_clear(Interaction *self) {
  g_mutex_lock(&self->lock);
  if (self->expression)
    g_string_free(self->expression, TRUE);
  if (self->result)
    g_string_free(self->result, TRUE);
  if (self->output)
    g_string_free(self->output, TRUE);
  if (self->error)
    g_string_free(self->error, TRUE);
  self->done_cb = NULL;
  self->done_cb_data = NULL;
  g_mutex_unlock(&self->lock);
  g_mutex_clear(&self->lock);
}

