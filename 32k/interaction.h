#ifndef INTERACTION_H
#define INTERACTION_H

#include <glib.h>

typedef enum {
  INTERACTION_CREATED,
  INTERACTION_RUNNING,
  INTERACTION_OK,
  INTERACTION_ERROR
} InteractionStatus;

typedef struct {
  gchar *expression;
  InteractionStatus status;
  gchar *result;
  gchar *output;
  gchar *errors;
} Interaction;

static inline void interaction_init(Interaction *self, const gchar *expr) {
  self->expression = g_strdup(expr);
  self->status = INTERACTION_CREATED;
  self->result = NULL;
  self->output = NULL;
  self->errors = NULL;
}

static inline void interaction_clear(Interaction *self) {
  g_free(self->expression);
  g_free(self->result);
  g_free(self->output);
  g_free(self->errors);
}

#endif /* INTERACTION_H */
