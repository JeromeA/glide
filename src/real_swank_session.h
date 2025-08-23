#pragma once

#include "swank_session.h"
#include "swank_process.h"
#include "status_service.h"

typedef struct {
  SwankSession base;
  SwankProcess *proc;
  StatusService *status_service;
  gboolean started;
  guint32 next_tag;
  GHashTable *interactions;
  GAsyncQueue *queue;
  GThread *thread;
  GMutex lock;
  SwankSessionCallback added_cb;
  gpointer added_cb_data;
  SwankSessionCallback updated_cb;
  gpointer updated_cb_data;
} RealSwankSession;

SwankSession *real_swank_session_new(SwankProcess *proc, StatusService *status_service);
void real_swank_session_on_message(GString *msg, gpointer user_data);

