#ifndef REAL_SWANK_PROCESS_H
#define REAL_SWANK_PROCESS_H

#include <glib.h>
#include <gio/gio.h> // For GSocketConnection, GSocketClient

// Callback type for Swank messages
typedef void (*GlobalSwankProcessMessageCallback)(GString *msg, gpointer user_data);

// Initializes the global Swank process state.
// It will internally use global preferences (for port) and global real_process functions.
void real_swank_process_init_globals();

// Starts the global Swank process (which includes starting the underlying real_process if not already started)
void real_swank_process_global_start();

// Sends a payload to the global Swank process
void real_swank_process_global_send(const GString *payload);

// Sets the message callback for the global Swank process
void real_swank_process_global_set_message_cb(GlobalSwankProcessMessageCallback cb, gpointer user_data);

// Allows setting the Swank socket FD directly (e.g., for testing or alternative connection methods)
void real_swank_process_global_set_socket_fd(int fd);

// Cleans up global Swank process resources
void real_swank_process_cleanup_globals();

#endif /* REAL_SWANK_PROCESS_H */
