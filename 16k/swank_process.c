#include "swank_process.h"
#include "swank_session.h" // For swank_session_on_message_internal
#include "preferences.h"  // For global preferences functions
#include "syscalls.h"     // For sys_read, sys_write
#include <gio/gio.h>
#include <unistd.h>    // For close, setsid
#include <errno.h>
#include <string.h>    // For strlen, strstr, memcpy
#include <signal.h>    // For SIGKILL
#include <sys/prctl.h> // For prctl
#include <sys/wait.h>  // For waitpid (though g_spawn_close_pid is used)
#include <stdio.h>     // For perror

// Define global static variables for the Process's state
static gint g_process_in_fd = -1;  // Parent's write end to child's stdin
static gint g_process_out_fd = -1; // Parent's read end from child's stdout
static gint g_process_err_fd = -1; // Parent's read end from child's stderr
static gchar **g_process_argv = NULL;

static void on_lisp_stdout(gchar *data, int len);

// Thread function to read stdout
static gpointer stdout_thread(gpointer /*data*/) {
  char buf[256];
  ssize_t n = 0;
  while ((n = sys_read(g_process_out_fd, buf, sizeof(buf))) > 0) {
    on_lisp_stdout(buf, n);
  }
  return NULL;
}

// Thread function to read stderr
static gpointer stderr_thread(gpointer /*data*/) {
  char buf[256];
  int n = 0;
  while ((n = sys_read(g_process_err_fd, buf, sizeof(buf))) > 0) {
    g_printerr("LISP STDERR: %.*s\n", n, buf);
  }
  return NULL;
}

// g_spawn_async_with_pipes child_setup function
static void child_setup(gpointer /*user_data*/) {
  setsid(); // Create a new session and set process group ID.
  prctl(PR_SET_PDEATHSIG, SIGKILL); // Ensure child dies if parent dies.
}

// Initialize process state from argv
static void process_init_from_argv(const gchar *const *argv) {
  g_strfreev(g_process_argv);
  g_process_argv = g_strdupv((gchar**)argv);
}

// Initialize process state from a single command
void process_init(const gchar *cmd) {
  const gchar *argv[] = { cmd, NULL };
  process_init_from_argv(argv);
}

void process_start() {
  GError *error = NULL;
  // G_SPAWN_SEARCH_PATH looks for the command in PATH.
  // G_SPAWN_DO_NOT_REAP_CHILD means we are responsible for the child process.
  // g_spawn_close_pid() will be used in cleanup.
  if (!g_spawn_async_with_pipes(NULL, // working directory (current)
                                g_process_argv,
                                NULL, // envp (current)
                                G_SPAWN_DO_NOT_REAP_CHILD | G_SPAWN_SEARCH_PATH,
                                child_setup, // function to run in child before exec
                                NULL, // user_data for child_setup
                                NULL,
                                &g_process_in_fd,  // child's stdin
                                &g_process_out_fd, // child's stdout
                                &g_process_err_fd, // child's stderr
                                &error)) {
    g_printerr("lisp start fail: %s\n", error ? error->message : "Unknown error");
    g_clear_error(&error);
    // Cleanup partially initialized state if spawn fails
    g_strfreev(g_process_argv);
    return;
  }

  g_thread_new("process-stdout", stdout_thread, NULL);
  g_thread_new("process-stderr", stderr_thread, NULL);
}

void process_write(const gchar *data, gssize len) {
  if (len < 0) {
    len = strlen(data);
  }
  ssize_t written = sys_write(g_process_in_fd, data, len);
  if (written == -1) {
      g_printerr("lisp write error: %s\n", g_strerror(errno));
  }
}

// Static variables for SwankProcess's state (distinct from general process)
static gint g_swank_fd = -1;
static GSocketConnection *g_swank_connection = NULL; // Store the connection to manage its lifecycle

static GString *g_swank_out_buffer = NULL;    // Buffer for stdout from the underlying Lisp process (before Swank starts)
static gsize    g_swank_out_consumed = 0;
static GMutex   g_swank_out_mutex;        // To protect g_swank_out_buffer and g_swank_out_consumed
static GCond    g_swank_out_cond;         // To signal new data in g_swank_out_buffer

static GString *g_swank_incoming_data_buffer = NULL; // Buffer for data coming directly from Swank socket
static gsize    g_swank_incoming_consumed = 0;
static GMutex   g_swank_incoming_mutex;   // To protect g_swank_incoming_data_buffer and g_swank_incoming_consumed

void swank_process_init() {

    // Initialize mutexes and cond var
    g_mutex_init(&g_swank_out_mutex);
    g_cond_init(&g_swank_out_cond);
    g_mutex_init(&g_swank_incoming_mutex);

    // Initialize buffers
    g_swank_out_buffer = g_string_new(NULL);
    g_swank_incoming_data_buffer = g_string_new(NULL);

}

void on_lisp_stdout(gchar *buf, int n) {
    g_mutex_lock(&g_swank_out_mutex);
    g_string_append_len(g_swank_out_buffer, buf, n);
    g_cond_signal(&g_swank_out_cond); // Signal that new data is available
    g_mutex_unlock(&g_swank_out_mutex);
}

static gpointer swank_reader_thread(gpointer /*data*/) {
    char buf[1024]; // Read buffer
    ssize_t n_read;

    while (g_swank_fd >= 0) { // Loop as long as the Swank FD is valid
        n_read = sys_read(g_swank_fd, buf, sizeof(buf));
        if (n_read > 0) {
            char *dbg_str = g_strndup(buf, n_read);
            g_free(dbg_str);

            g_mutex_lock(&g_swank_incoming_mutex);
            g_string_append_len(g_swank_incoming_data_buffer, buf, n_read);

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

                            // Dispatch the message by directly calling swank_session_on_message_internal
                            swank_session_on_message_internal(actual_msg, NULL); // g_swank_message_cb_data was NULL
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
            // } // End of removed if(g_swank_message_cb)
             // Compact the buffer if a lot has been consumed
            if (g_swank_incoming_consumed > 0 && g_swank_incoming_data_buffer->len > g_swank_incoming_consumed) {
                g_string_erase(g_swank_incoming_data_buffer, 0, g_swank_incoming_consumed);
            } else if (g_swank_incoming_consumed == g_swank_incoming_data_buffer->len) {
                 g_string_set_size(g_swank_incoming_data_buffer, 0);
            }
            g_swank_incoming_consumed = 0; // Reset after processing or compaction
            g_mutex_unlock(&g_swank_incoming_mutex);

        } else if (n_read == 0) { // EOF
            break;
        } else { // Error
            if (errno == EAGAIN || errno == EWOULDBLOCK) {
                g_usleep(10000); // Sleep a bit and retry
                continue;
            }
            g_printerr("swank_process: swank_reader_thread read error on fd %d: %s (errno %d)\n", g_swank_fd, g_strerror(errno), errno);
            break;
        }
    }
    // FD closure is handled by the cleanup function.
    return NULL;
}

// Reads from the Lisp process's stdout (g_swank_out_buffer) until pattern is found
static void read_until_from_lisp_output(const char *pattern) {
    const gsize pattern_len = strlen(pattern);
    g_mutex_lock(&g_swank_out_mutex);
    while (TRUE) {
        const char *buffer_start = g_swank_out_buffer->str + g_swank_out_consumed;
        const char *found_at = strstr(buffer_start, pattern);

        if (found_at) {
            g_swank_out_consumed = (found_at - g_swank_out_buffer->str) + pattern_len;
            // Optional: Compact g_swank_out_buffer if needed
            if (g_swank_out_consumed == g_swank_out_buffer->len) {
                g_string_set_size(g_swank_out_buffer, 0);
                g_swank_out_consumed = 0;
            }
            g_mutex_unlock(&g_swank_out_mutex);
            return;
        }
        // Wait for more data to arrive in on_lisp_stdout
        g_cond_wait(&g_swank_out_cond, &g_swank_out_mutex);
    }
}

static void start_lisp_and_swank_server() {

    process_start(); // Start the Lisp process

    // Wait for Lisp prompt (e.g., "* ")
    read_until_from_lisp_output("* "); // This pattern may need to be more robust or configurable.

    // This command depends on the Lisp implementation (e.g., ql:quickload vs require).
    // Using (require :swank) for now.
    const char *load_swank_cmd = "(require :swank)\n";
    process_write(load_swank_cmd, -1);

    read_until_from_lisp_output(")"); // A simple heuristic: wait for some closing paren. This needs improvement.
    read_until_from_lisp_output("* "); // Wait for prompt again

    char create_server_cmd[128];
    g_snprintf(create_server_cmd, sizeof(create_server_cmd),
               "(swank:create-server :port 4005 :dont-close t)\n");
    process_write(create_server_cmd, -1);

    // Wait for Swank server to report its port or for prompt again
    read_until_from_lisp_output("* "); // Or a specific message like ";; Swank started on port XXXX."
}

static void connect_to_swank_server() {
    GSocketClient *client = g_socket_client_new();
    GError *conn_error = NULL;

    // Loop with retries for connection
    for (int i=0; i < 10; ++i) { // Try for ~5 seconds
        g_swank_connection = g_socket_client_connect_to_host(client,
                                                             "127.0.0.1",
                                                             4005,
                                                             NULL, // GCancellable
                                                             &conn_error);
        if (g_swank_connection) {
            break; // Success
        }
        g_clear_error(&conn_error);
        g_usleep(500000); // Wait 0.5 sec before retrying
    }
    g_object_unref(client);

    if (!g_swank_connection) {
        g_printerr("swank_process: Failed to connect to Swank server on port 4005 after multiple retries.\n");
        if(conn_error) g_clear_error(&conn_error);
        return; // Failed to connect
    }

    GSocket *socket = g_socket_connection_get_socket(g_swank_connection);
    g_swank_fd = g_socket_get_fd(socket); // We don't own the FD from GSocket, GSocketConnection manages it.
                                        // However, sys_read needs an FD.
                                        // For GSocket, it's better to use g_input_stream_read.
                                        // But to keep sys_read, we get the FD. Be careful with its lifecycle.
                                        // GSocket usually makes it non-blocking.

    g_thread_new("swank-reader", swank_reader_thread, NULL);
}

void swank_process_start() {
    start_lisp_and_swank_server(); // This starts the underlying Lisp process via process_start()
    connect_to_swank_server();
}

void swank_process_send(const GString *payload) {
    size_t len = payload->len;
    char hdr[7]; // 6 hex chars for length + null terminator
    g_snprintf(hdr, sizeof(hdr), "%06zx", len); // Format length as 6-digit hex


    ssize_t nw_hdr = sys_write(g_swank_fd, hdr, 6);
    if (nw_hdr != 6) {
        g_printerr("lisp write fail: %d\n", errno);
        return;
    }

    ssize_t nw_payload = sys_write(g_swank_fd, payload->str, len);
    if (nw_payload != (ssize_t)len) {
        g_printerr("lisp write fail: %d\n", errno);
        return;
    }
}

