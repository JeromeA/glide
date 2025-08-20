#ifndef INTERACTION_H
#define INTERACTION_H

#include <glib.h>

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

typedef struct {
  gchar *expression;
  guint32 tag;
  InteractionStatus status;
  InteractionType type;
  gchar *result;
  gchar *output;
  gchar *error;
} Interaction;

static inline void interaction_init(Interaction *self, const gchar *expr) {
  self->expression = g_strdup(expr);
  self->tag = 0;
  self->status = INTERACTION_CREATED;
  self->type = INTERACTION_USER;
  self->result = NULL;
  self->output = NULL;
  self->error = NULL;
}

static inline void interaction_clear(Interaction *self) {
  g_free(self->expression);
  g_free(self->result);
  g_free(self->output);
  g_free(self->error);
}

#endif /* INTERACTION_H */
