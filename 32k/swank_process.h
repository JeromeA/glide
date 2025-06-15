#ifndef SWANK_PROCESS_H
#define SWANK_PROCESS_H

#include "preferences.h"
#include "process.h"

#include <glib.h>

typedef struct _SwankProcess SwankProcess;           /* interface */
typedef struct _SwankProcessImpl SwankProcessImpl;   /* implementation */

struct _SwankProcess {
  void     (*send)(SwankProcessImpl *self, const GString *payload);
  GString *(*get_reply)(SwankProcessImpl *self);
  void     (*free)(SwankProcessImpl *self);
};

struct _SwankProcessImpl {
  const SwankProcess *iface;
};

SwankProcessImpl *swank_process_new(ProcessImpl *proc, Preferences *prefs);
SwankProcessImpl *swank_process_new_from_fd(int fd); /* for testing */
void swank_process_send(SwankProcessImpl *self, const GString *payload);
GString *swank_process_get_reply(SwankProcessImpl *self);
void swank_process_free(SwankProcessImpl *self);

#endif /* SWANK_PROCESS_H */
