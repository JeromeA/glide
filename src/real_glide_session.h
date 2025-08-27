#pragma once

#include "glide_session.h"
#include "glide_process.h"
#include "status_service.h"

typedef struct {
  GlideSession base;
  GlideProcess *proc;
  StatusService *status_service;
  gboolean started;
  GAsyncQueue *queue;
  GThread *thread;
  GMutex lock;
  GCond cond;
  Interaction *current;
  GlideSessionCallback added_cb;
  gpointer added_cb_data;
  GlideSessionCallback updated_cb;
  gpointer updated_cb_data;
} RealGlideSession;

GlideSession *real_glide_session_new(GlideProcess *proc, StatusService *status_service);
void real_glide_session_on_message(GString *msg, gpointer user_data);

