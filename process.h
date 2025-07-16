#ifndef PROCESS_H
#define PROCESS_H

#include <glib-object.h>

typedef struct _Process Process;

typedef void (*ProcessCallback)(GString *data, gpointer user_data);

#define PROCESS_TYPE (process_get_type())
G_DECLARE_INTERFACE(Process, process, GLIDE, PROCESS, GObject)

struct _ProcessInterface {
  GTypeInterface parent_iface;
  void     (*start)(Process *self);
  void     (*set_stdout_cb)(Process *self, ProcessCallback cb, gpointer user_data);
  void     (*set_stderr_cb)(Process *self, ProcessCallback cb, gpointer user_data);
  gboolean (*write)(Process *self, const gchar *data, gssize len);
};

static inline void process_set_stdout_cb(Process *self, ProcessCallback cb, gpointer user_data) {
  g_return_if_fail(GLIDE_IS_PROCESS(self));
  GLIDE_PROCESS_GET_IFACE(self)->set_stdout_cb(self, cb, user_data);
}

static inline void process_set_stderr_cb(Process *self, ProcessCallback cb, gpointer user_data) {
  g_return_if_fail(GLIDE_IS_PROCESS(self));
  GLIDE_PROCESS_GET_IFACE(self)->set_stderr_cb(self, cb, user_data);
}

static inline gboolean process_write(Process *self, const gchar *data, gssize len) {
  g_return_val_if_fail(GLIDE_IS_PROCESS(self), FALSE);
  return GLIDE_PROCESS_GET_IFACE(self)->write(self, data, len);
}

static inline void process_start(Process *self) {
  g_return_if_fail(GLIDE_IS_PROCESS(self));
  GLIDE_PROCESS_GET_IFACE(self)->start(self);
}

#endif /* PROCESS_H */
