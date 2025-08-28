#pragma once

#include "repl_process.h"
#include "interaction.h"
#include "status_service.h"

#include <glib.h>

typedef struct _ReplSession ReplSession;
typedef void (*ReplSessionCallback)(ReplSession *self, Interaction *interaction, gpointer user_data);

ReplSession *repl_session_new(ReplProcess *proc, StatusService *status_service);
void          repl_session_eval(ReplSession *self, Interaction *interaction);
void          repl_session_set_interaction_added_cb(ReplSession *self, ReplSessionCallback cb, gpointer user_data);
void          repl_session_set_interaction_updated_cb(ReplSession *self, ReplSessionCallback cb, gpointer user_data);
ReplSession *repl_session_ref(ReplSession *self);
void          repl_session_unref(ReplSession *self);

void          repl_session_on_message(GString *msg, gpointer user_data);
