#ifndef PROCESS_H
#define PROCESS_H

#include <glib.h> // For gchar, gpointer, gboolean, GPid, GThread, etc.

// Callback type for stdout/stderr data
typedef void (*GlobalProcessCallback)(GString *data, gpointer user_data);

// Initializes the global process a single command string
void process_init_globals(const gchar *cmd);

// Initializes the global process from an argument vector
void process_init_globals_from_argv(const gchar *const *argv);

// Sets the callback for stdout
void process_global_set_stdout_cb(GlobalProcessCallback cb, gpointer user_data);

// Sets the callback for stderr
void process_global_set_stderr_cb(GlobalProcessCallback cb, gpointer user_data);

// Writes data to the process's stdin
gboolean process_global_write(const gchar *data, gssize len);

// Starts the process execution
void process_global_start();

// Cleans up global process resources (e.g., at application exit)
void process_cleanup_globals();

#endif /* PROCESS_H */
