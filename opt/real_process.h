#ifndef REAL_PROCESS_H
#define REAL_PROCESS_H

#include <glib.h> // For gchar, gpointer, gboolean, GPid, GThread, etc.
// process.h has been removed

// Callback type for stdout/stderr data
typedef void (*GlobalProcessCallback)(GString *data, gpointer user_data);

// Initializes the global process a single command string
void real_process_init_globals(const gchar *cmd);

// Initializes the global process from an argument vector
void real_process_init_globals_from_argv(const gchar *const *argv);

// Sets the callback for stdout
void real_process_global_set_stdout_cb(GlobalProcessCallback cb, gpointer user_data);

// Sets the callback for stderr
void real_process_global_set_stderr_cb(GlobalProcessCallback cb, gpointer user_data);

// Writes data to the process's stdin
gboolean real_process_global_write(const gchar *data, gssize len);

// Starts the process execution
void real_process_global_start();

// Cleans up global process resources (e.g., at application exit)
void real_process_cleanup_globals();

#endif /* REAL_PROCESS_H */
