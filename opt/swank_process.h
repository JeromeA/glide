#ifndef SWANK_PROCESS_H
#define SWANK_PROCESS_H

#include <glib.h> // For gchar, gpointer, gboolean, GPid, GThread, etc.
#include <gio/gio.h> // For GSocketConnection, GSocketClient

// Initializes the process a single command string
void process_init(const gchar *cmd);

// process_init_from_argv is now static

// Writes data to the process's stdin
void process_write(const gchar *data, gssize len);

// Starts the process execution
void process_start();

// Cleans up process resources (e.g., at application exit)
void process_cleanup();

// Initializes the Swank process state.
// It will internally use preferences (for port) and process functions.
void swank_process_init();

// Starts the Swank process (which includes starting the underlying process if not already started)
void swank_process_start();

// Sends a payload to the Swank process
void swank_process_send(const GString *payload);

// Cleans up Swank process resources
void swank_process_cleanup();

#endif /* SWANK_PROCESS_H */
