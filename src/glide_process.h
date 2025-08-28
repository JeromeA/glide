#pragma once

#include "process.h"
#include <glib.h>

typedef struct _GlideProcess GlideProcess;
typedef void (*GlideProcessMessageCallback)(GString *msg, gpointer user_data);

struct _GlideProcess {
  int refcnt;
  Process *proc;
  GString *buffer;
  GMutex mutex;
  GCond cond;
  GlideProcessMessageCallback msg_cb;
  gpointer msg_cb_data;
  gboolean started;
  int start_state;
};

GlideProcess *glide_process_new(Process *proc);
void          glide_process_start(GlideProcess *self);
void          glide_process_send(GlideProcess *self, const GString *payload);
void          glide_process_set_message_cb(GlideProcess *self, GlideProcessMessageCallback cb, gpointer user_data);
GlideProcess *glide_process_ref(GlideProcess *self);
void          glide_process_unref(GlideProcess *self);
