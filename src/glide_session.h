#pragma once

#include "glide_process.h"
#include "interaction.h"
#include "status_service.h"

#include <glib.h>

typedef struct _GlideSession GlideSession;
typedef void (*GlideSessionCallback)(GlideSession *self, Interaction *interaction, gpointer user_data);

GlideSession *glide_session_new(GlideProcess *proc, StatusService *status_service);
void          glide_session_eval(GlideSession *self, Interaction *interaction);
void          glide_session_set_interaction_added_cb(GlideSession *self, GlideSessionCallback cb, gpointer user_data);
void          glide_session_set_interaction_updated_cb(GlideSession *self, GlideSessionCallback cb, gpointer user_data);
GlideSession *glide_session_ref(GlideSession *self);
void          glide_session_unref(GlideSession *self);

void          glide_session_on_message(GString *msg, gpointer user_data);
