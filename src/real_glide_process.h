#pragma once

#include "glide_process.h"

typedef struct {
  GlideProcess base;
  Process *proc;
  GString *buffer;
  GMutex mutex;
  GCond cond;
  GlideProcessMessageCallback msg_cb;
  gpointer msg_cb_data;
  gboolean started;
  int start_state;
} RealGlideProcess;

GlideProcess *real_glide_process_new(Process *proc);

