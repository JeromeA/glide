#pragma once

#include "swank_process.h"
#include <gio/gio.h>

typedef struct _Preferences Preferences;

typedef struct {
  SwankProcess base;
  Process *proc;
  Preferences *prefs;
  int swank_fd;
  GSocketConnection *connection;
  GString *out_data;
  gsize out_consumed;
  GMutex out_mutex;
  GCond  out_cond;
  GString *swank_data;
  gsize swank_consumed;
  GMutex swank_mutex;
  SwankProcessMessageCallback msg_cb;
  gpointer msg_cb_data;
  int port;
  GThread *swank_thread;
  gboolean started;
} RealSwankProcess;

SwankProcess *real_swank_process_new(Process *proc, Preferences *prefs);
void real_swank_process_set_socket(RealSwankProcess *self, int fd);

