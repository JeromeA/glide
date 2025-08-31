#pragma once

#include "process.h"
#include <glib.h>

typedef struct _ReplProcess ReplProcess;
typedef void (*ReplProcessMessageCallback)(GString *msg, gpointer user_data);

ReplProcess *repl_process_new(Process *proc);
void          repl_process_start(ReplProcess *self);
gboolean      repl_process_send(ReplProcess *self, const GString *payload);
void          repl_process_set_message_cb(ReplProcess *self, ReplProcessMessageCallback cb, gpointer user_data);
ReplProcess *repl_process_ref(ReplProcess *self);
void          repl_process_unref(ReplProcess *self);
