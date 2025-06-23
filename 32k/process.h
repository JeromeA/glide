#ifndef PROCESS_H
#define PROCESS_H

#include <glib.h>

typedef struct _Process Process;       /* interface */
typedef struct _ProcessImpl ProcessImpl; /* implementation */

typedef void (*ProcessCallback)(GString *data, gpointer user_data);

struct _Process {
  void     (*set_stdout_cb)(ProcessImpl *proc, ProcessCallback cb, gpointer user_data);
  void     (*set_stderr_cb)(ProcessImpl *proc, ProcessCallback cb, gpointer user_data);
  gboolean (*write)(ProcessImpl *proc, const gchar *data, gssize len);
  void     (*free)(ProcessImpl *proc);
};

struct _ProcessImpl {
  const Process *iface;
};

ProcessImpl *process_new(const gchar *cmd);
ProcessImpl *process_new_from_argv(const gchar *const *argv);
void process_set_stdout_cb(ProcessImpl *proc, ProcessCallback cb, gpointer user_data);
void process_set_stderr_cb(ProcessImpl *proc, ProcessCallback cb, gpointer user_data);
gboolean process_write(ProcessImpl *proc, const gchar *data, gssize len);
void process_free(ProcessImpl *proc);

#endif /* PROCESS_H */
