#include "swank_process.h"
#include "process.h" // For global process functions
#include "preferences.h"  // For global preferences functions
#include "syscalls.h"     // For sys_read, sys_write
#include "util.h"         // For g_debug_40

#include <gio/gio.h>
#include <unistd.h>    // For close
#include <string.h>    // For strlen, strstr, memcpy
#include <errno.h>     // For errno

// Global static variables for SwankProcess's state
static gint g_swank_fd = -1;
static GSocketConnection *g_swank_connection = NULL; // Store the connection to manage its lifecycle

static GString *g_swank_out_buffer = NULL;    // Buffer for stdout from the underlying Lisp process (before Swank starts)
static gsize    g_swank_out_consumed = 0;
static GMutex   g_swank_out_mutex;        // To protect g_swank_out_buffer and g_swank_out_consumed
static GCond    g_swank_out_cond;         // To signal new data in g_swank_out_buffer

static GString *g_swank_incoming_data_buffer = NULL; // Buffer for data coming directly from Swank socket
static gsize    g_swank_incoming_consumed = 0;
static GMutex   g_swank_incoming_mutex;   // To protect g_swank_incoming_data_buffer and g_swank_incoming_consumed

static GlobalSwankProcessMessageCallback g_swank_message_cb = NULL;
static gpointer g_swank_message_cb_data = NULL;

static gint    g_swank_port_number = 4005; // Default, will be updated from global preferences
static GThread *g_swank_reader_thread = NULL;
static gboolean g_swank_process_started = FALSE;


// --- Forward declarations for internal static functions ---
static gpointer swank_reader_thread_global(gpointer data);
static void read_until_from_lisp_output(const char *pattern);
static void on_lisp_stdout(GString *data, gpointer user_data);
static void on_lisp_stderr(GString *data, gpointer user_data);
static void start_lisp_and_swank_server();
static void connect_to_swank_server();

void swank_process_init_globals() {
    g_debug("swank_process_init_globals");

    if (g_swank_process_started) {
        g_warning("swank_process_init_globals: Already initialized. Cleaning up old state.");
        swank_process_cleanup_globals();
    }

    // Initialize mutexes and cond var
    g_mutex_init(&g_swank_out_mutex);
    g_cond_init(&g_swank_out_cond);
    g_mutex_init(&g_swank_incoming_mutex);

    // Initialize buffers
    g_swank_out_buffer = g_string_new(NULL);
    g_swank_out_consumed = 0;
    g_swank_incoming_data_buffer = g_string_new(NULL);
    g_swank_incoming_consumed = 0;

    g_swank_fd = -1;
    g_swank_connection = NULL;
    g_swank_message_cb = NULL;
    g_swank_message_cb_data = NULL;
    g_swank_reader_thread = NULL;

    // Get Swank port from global preferences
    g_swank_port_number = preferences_get_swank_port_global();
    if (g_swank_port_number == 0) { // Simple check if port is uninitialized or invalid from prefs
        g_swank_port_number = 4005; // Fallback
        g_warning("swank_process_init_globals: Invalid Swank port from preferences, using default %d", g_swank_port_number);
    }

    // Set up callbacks for the underlying Lisp process output
    process_global_set_stdout_cb(on_lisp_stdout, NULL);
    process_global_set_stderr_cb(on_lisp_stderr, NULL);

    g_swank_process_started = FALSE; // Will be set to TRUE in _global_start
    g_debug("swank_process_init_globals: complete. Port: %d", g_swank_port_number);
}

static gpointer swank_reader_thread_global(gpointer /*data*/) {
    g_debug("swank_process: swank_reader_thread_global starting for fd %d", g_swank_fd);
    char buf[1024]; // Read buffer
    ssize_t n_read;

    while (g_swank_fd >= 0) { // Loop as long as the Swank FD is valid
        n_read = sys_read(g_swank_fd, buf, sizeof(buf));
        if (n_read > 0) {
            char *dbg_str = g_strndup(buf, n_read);
            g_debug_40("swank_process: swank_reader_thread_global received data:", dbg_str);
            g_free(dbg_str);

            g_mutex_lock(&g_swank_incoming_mutex);
            g_string_append_len(g_swank_incoming_data_buffer, buf, n_read);

            if (g_swank_message_cb) {
                while (TRUE) { // Process all complete messages in buffer
                    if (g_swank_incoming_data_buffer->len - g_swank_incoming_consumed >= 6) { // Enough for header?
                        char hdr[7];
                        memcpy(hdr, g_swank_incoming_data_buffer->str + g_swank_incoming_consumed, 6);
                        hdr[6] = '\0';
                        gsize msg_len = g_ascii_strtoull(hdr, NULL, 16); // Hex string to size_t

                        if (g_swank_incoming_data_buffer->len - g_swank_incoming_consumed - 6 >= msg_len) { // Full message body available?
                            char *msg_start_ptr = g_swank_incoming_data_buffer->str + g_swank_incoming_consumed + 6;
                            GString *actual_msg = g_string_new_len(msg_start_ptr, msg_len);

                            g_swank_incoming_consumed += (6 + msg_len);

                            // Dispatch the message
                            g_swank_message_cb(actual_msg, g_swank_message_cb_data);
                            // The original RealSwankProcess freed the GString after the callback, so we follow that.
                            g_string_free(actual_msg, TRUE);

                            // If buffer fully consumed, reset pointers for efficiency
                            if (g_swank_incoming_consumed == g_swank_incoming_data_buffer->len) {
                                g_string_set_size(g_swank_incoming_data_buffer, 0);
                                g_swank_incoming_consumed = 0;
                            }
                            continue; // Check for more messages
                        }
                    }
                    break; // Not enough data for header or full message body
                }
            }
             // Compact the buffer if a lot has been consumed
            if (g_swank_incoming_consumed > 0 && g_swank_incoming_data_buffer->len > g_swank_incoming_consumed) {
                g_string_erase(g_swank_incoming_data_buffer, 0, g_swank_incoming_consumed);
            } else if (g_swank_incoming_consumed == g_swank_incoming_data_buffer->len) {
                 g_string_set_size(g_swank_incoming_data_buffer, 0);
            }
            g_swank_incoming_consumed = 0; // Reset after processing or compaction
            g_mutex_unlock(&g_swank_incoming_mutex);

        } else if (n_read == 0) { // EOF
            g_debug("swank_process: swank_reader_thread_global: EOF on Swank FD %d", g_swank_fd);
            break;
        } else { // Error
            if (errno == EAGAIN || errno == EWOULDBLOCK) {
                g_usleep(10000); // Sleep a bit and retry
                continue;
            }
            g_printerr("swank_process: swank_reader_thread_global read error on fd %d: %s (errno %d)\n", g_swank_fd, g_strerror(errno), errno);
            break;
        }
    }
    g_debug("swank_process: swank_reader_thread_global exiting for fd %d", g_swank_fd);
    // FD closure is handled by the cleanup function.
    return NULL;
}

// Reads from the Lisp process's stdout (g_swank_out_buffer) until pattern is found
static void read_until_from_lisp_output(const char *pattern) {
    g_debug("swank_process: read_until_from_lisp_output: waiting for '%s'", pattern);
    const gsize pattern_len = strlen(pattern);
    g_mutex_lock(&g_swank_out_mutex);
    while (TRUE) {
        const char *buffer_start = g_swank_out_buffer->str + g_swank_out_consumed;
        const char *found_at = strstr(buffer_start, pattern);

        if (found_at) {
            g_swank_out_consumed = (found_at - g_swank_out_buffer->str) + pattern_len;
            g_debug("swank_process: read_until_from_lisp_output: found '%s'. Consumed up to %zu.", pattern, g_swank_out_consumed);
            // Optional: Compact g_swank_out_buffer if needed
            if (g_swank_out_consumed == g_swank_out_buffer->len) {
                g_string_set_size(g_swank_out_buffer, 0);
                g_swank_out_consumed = 0;
            }
            g_mutex_unlock(&g_swank_out_mutex);
            return;
        }
        // Wait for more data to arrive in on_lisp_stdout
        g_debug("swank_process: read_until_from_lisp_output: pattern not found, waiting on g_swank_out_cond.");
        g_cond_wait(&g_swank_out_cond, &g_swank_out_mutex);
        g_debug("swank_process: read_until_from_lisp_output: woken up.");
    }
    // g_mutex_unlock(&g_swank_out_mutex); // Should be unlocked before return in loop
}

// Callback for stdout from the underlying Lisp process
static void on_lisp_stdout(GString *data, gpointer /*user_data*/) {
    g_debug_40("swank_process: on_lisp_stdout received:", data->str);
    g_mutex_lock(&g_swank_out_mutex);
    g_string_append_len(g_swank_out_buffer, data->str, data->len);
    g_cond_signal(&g_swank_out_cond); // Signal that new data is available
    g_mutex_unlock(&g_swank_out_mutex);
}

// Callback for stderr from the underlying Lisp process
static void on_lisp_stderr(GString *data, gpointer /*user_data*/) {
    g_printerr("LISP STDERR: %s\n", data->str); // Print Lisp's stderr directly
}

static void start_lisp_and_swank_server() {
    g_debug("swank_process: start_lisp_and_swank_server");

    process_global_start(); // Start the Lisp process

    // Wait for Lisp prompt (e.g., "* ")
    read_until_from_lisp_output("* "); // This pattern may need to be more robust or configurable.

    // This command depends on the Lisp implementation (e.g., ql:quickload vs require).
    // Using (require :swank) for now.
    const char *load_swank_cmd = "(require :swank)\n";
    g_debug("swank_process: Sending Lisp command: %s", load_swank_cmd);
    process_global_write(load_swank_cmd, -1);

    // Wait for confirmation of Swank loading. This is highly Lisp-dependent.
    // Example for SBCL: ("SB-INTROSPECT" "SB-CLTL2")
    // Example for CCL: (:SWANK)
    // For now, using a generic prompt again or a more specific pattern if known.
    read_until_from_lisp_output(")"); // A simple heuristic: wait for some closing paren. This needs improvement.
    read_until_from_lisp_output("* "); // Wait for prompt again

    char create_server_cmd[128];
    g_snprintf(create_server_cmd, sizeof(create_server_cmd),
               "(swank:create-server :port %d :dont-close t)\n", g_swank_port_number);
    g_debug("swank_process: Sending Lisp command: %s", create_server_cmd);
    process_global_write(create_server_cmd, -1);

    // Wait for Swank server to report its port or for prompt again
    read_until_from_lisp_output("* "); // Or a specific message like ";; Swank started on port XXXX."
    g_debug("swank_process: Swank server presumed started on Lisp side.");
}

static void connect_to_swank_server() {
    g_debug("swank_process: connect_to_swank_server trying port %d", g_swank_port_number);
    GSocketClient *client = g_socket_client_new();
    GError *conn_error = NULL;

    // Loop with retries for connection
    for (int i=0; i < 10; ++i) { // Try for ~5 seconds
        g_swank_connection = g_socket_client_connect_to_host(client,
                                                             "127.0.0.1",
                                                             g_swank_port_number,
                                                             NULL, // GCancellable
                                                             &conn_error);
        if (g_swank_connection) {
            break; // Success
        }
        g_debug("swank_process: Connection attempt %d failed: %s. Retrying...", i+1, conn_error ? conn_error->message : "Unknown error");
        g_clear_error(&conn_error);
        g_usleep(500000); // Wait 0.5 sec before retrying
    }
    g_object_unref(client);

    if (!g_swank_connection) {
        g_printerr("swank_process: Failed to connect to Swank server on port %d after multiple retries.\n", g_swank_port_number);
        if(conn_error) g_clear_error(&conn_error);
        return; // Failed to connect
    }

    GSocket *socket = g_socket_connection_get_socket(g_swank_connection);
    g_swank_fd = g_socket_get_fd(socket); // We don't own the FD from GSocket, GSocketConnection manages it.
                                        // However, sys_read needs an FD.
                                        // For GSocket, it's better to use g_input_stream_read.
                                        // But to keep sys_read, we get the FD. Be careful with its lifecycle.
                                        // GSocket usually makes it non-blocking.

    // Make the socket blocking for sys_read if necessary, or adapt sys_read logic.
    // For simplicity, we assume sys_read handles non-blocking behavior or FD is blocking.
    // If GSocket FD is non-blocking by default, sys_read might return EAGAIN.
    // The swank_reader_thread_global has a basic EAGAIN check.

    g_debug("swank_process: Connected to Swank server. FD: %d", g_swank_fd);

    // Start the Swank message reader thread
    if (g_swank_fd >=0 && !g_swank_reader_thread) {
        g_swank_reader_thread = g_thread_new("swank-reader", swank_reader_thread_global, NULL);
    }
}

void swank_process_global_start() {
    g_debug("swank_process_global_start");
    if (g_swank_process_started) {
        g_warning("swank_process_global_start: Swank process already started.");
        return;
    }

    start_lisp_and_swank_server(); // This starts the underlying Lisp process via process_global_start()
    connect_to_swank_server();

    if (g_swank_fd >= 0) { // Successfully connected
        g_swank_process_started = TRUE;
        g_debug("swank_process_global_start: Swank process started successfully.");
    } else {
        g_printerr("swank_process_global_start: Failed to start Swank process (connection failed).\n");
        // Perform partial cleanup if connection failed after Lisp started
        swank_process_cleanup_globals(); // This will also cleanup the underlying process
    }
}

void swank_process_global_send(const GString *payload) {
    if (!g_swank_process_started || g_swank_fd < 0) {
        g_warning("swank_process_global_send: Swank process not started or FD invalid.");
        return;
    }

    size_t len = payload->len;
    char hdr[7]; // 6 hex chars for length + null terminator
    g_snprintf(hdr, sizeof(hdr), "%06zx", len); // Format length as 6-digit hex

    g_debug("swank_process_global_send: Sending Swank message: Header='%s', Payload='%.*s'", hdr, (int)len, payload->str);

    ssize_t nw_hdr = sys_write(g_swank_fd, hdr, 6);
    if (nw_hdr != 6) {
        g_printerr("swank_process_global_send: Failed to write Swank header (wrote %zd, errno %d)\n", nw_hdr, errno);
        // Consider this a critical failure; perhaps reset state or attempt reconnect.
        return;
    }

    ssize_t nw_payload = sys_write(g_swank_fd, payload->str, len);
    if (nw_payload != (ssize_t)len) {
        g_printerr("swank_process_global_send: Failed to write Swank payload (wrote %zd of %zu, errno %d)\n", nw_payload, len, errno);
        return;
    }
    g_debug("swank_process_global_send: Message sent successfully.");
}

void swank_process_global_set_message_cb(GlobalSwankProcessMessageCallback cb, gpointer user_data) {
    g_debug("swank_process_global_set_message_cb");
    g_mutex_lock(&g_swank_incoming_mutex);
    g_swank_message_cb = cb;
    g_swank_message_cb_data = user_data;
    g_mutex_unlock(&g_swank_incoming_mutex);
}

void swank_process_global_set_socket_fd(int fd) {
    g_debug("swank_process_global_set_socket_fd: Setting Swank FD to %d", fd);
    if (g_swank_fd >= 0 && g_swank_fd != fd) { // If there's an existing valid FD
        g_warning("swank_process_global_set_socket_fd: Closing existing Swank FD %d", g_swank_fd);
        close(g_swank_fd); // Close the old one
        if (g_swank_connection) {
             g_object_unref(g_swank_connection); // Release connection if we manage it
             g_swank_connection = NULL;
        }
    }
    g_swank_fd = fd;
    // If a reader thread was running on the old FD, it needs to be stopped and restarted for the new FD.
    // This function is a bit risky if not managed carefully with the reader thread.
    if (g_swank_reader_thread) {
        g_debug("swank_process_global_set_socket_fd: Existing reader thread found. It might need manual restart.");
        // For simplicity, current cleanup/init handles restarting reader thread if needed.
    }
}

void swank_process_cleanup_globals() {
    g_debug("swank_process_cleanup_globals: Starting cleanup.");

    // Close Swank connection and FD
    if (g_swank_fd >= 0) {
        g_debug("swank_process_cleanup_globals: Closing Swank FD %d.", g_swank_fd);
        // Prefer closing GSocketConnection first if it exists.
        if (g_swank_connection) {
            GError *close_err = NULL;
            g_io_stream_close(G_IO_STREAM(g_swank_connection), NULL, &close_err);
            if(close_err) {
                g_warning("Error closing GSocketConnection: %s", close_err->message);
                g_error_free(close_err);
            }
            g_object_unref(g_swank_connection);
            g_swank_connection = NULL;
        } else {
             // If no GSocketConnection, close FD directly (e.g. if set by _set_socket_fd)
            close(g_swank_fd);
        }
        g_swank_fd = -1; // Mark as closed to stop reader thread
    }

    // Join reader thread
    if (g_swank_reader_thread) {
        g_debug("swank_process_cleanup_globals: Joining Swank reader thread.");
        g_thread_join(g_swank_reader_thread); // This also unrefs the thread object
        g_swank_reader_thread = NULL;
    }

    // Cleanup of the underlying Lisp process (Process) is handled externally (e.g., in main.c).

    // Free buffers and other resources
    g_string_free(g_swank_out_buffer, TRUE); g_swank_out_buffer = NULL;
    g_string_free(g_swank_incoming_data_buffer, TRUE); g_swank_incoming_data_buffer = NULL;

    g_mutex_clear(&g_swank_out_mutex);
    g_cond_clear(&g_swank_out_cond);
    g_mutex_clear(&g_swank_incoming_mutex);

    g_swank_message_cb = NULL;
    g_swank_message_cb_data = NULL;
    g_swank_process_started = FALSE;

    g_debug("swank_process_cleanup_globals: Cleanup complete.");
}

// Note: Cleanup of global proc and prefs is handled by their respective units.
// This function focuses on resources directly managed by SwankProcess (sockets, threads, buffers).
