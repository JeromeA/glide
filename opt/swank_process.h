#ifndef SWANK_PROCESS_H
#define SWANK_PROCESS_H

#include <glib.h> // For gchar, gpointer, gboolean, GPid, GThread, etc.
#include <gio/gio.h> // For GSocketConnection, GSocketClient

// Removed typedef GlobalProcessCallback - Callbacks are hardcoded
// Removed typedef GlobalSwankProcessMessageCallback - Callback is hardcoded

// Initializes the global process a single command string
void process_init_globals(const gchar *cmd);

// process_init_globals_from_argv is now static

// Removed process_global_set_stdout_cb - Callback is hardcoded
// Removed process_global_set_stderr_cb - Callback is hardcoded

// Writes data to the process's stdin
gboolean process_global_write(const gchar *data, gssize len);

// Starts the process execution
void process_global_start();

// Cleans up global process resources (e.g., at application exit)
void process_cleanup_globals();

// Initializes the global Swank process state.
// It will internally use global preferences (for port) and global process functions.
void swank_process_init_globals();

// Starts the global Swank process (which includes starting the underlying process if not already started)
void swank_process_global_start();

// Sends a payload to the global Swank process
void swank_process_global_send(const GString *payload);

// Removed swank_process_global_set_message_cb - Callback is hardcoded
// Removed swank_process_global_set_socket_fd - Unused function

// Cleans up global Swank process resources
void swank_process_cleanup_globals();

#endif /* SWANK_PROCESS_H */
