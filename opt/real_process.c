#include "real_process.h"
#include "syscalls.h" // For sys_read, sys_write

#include <gio/gio.h>
#include <unistd.h>
#include <errno.h>
#include <string.h>
#include <signal.h>
#include <sys/prctl.h>
#include <sys/wait.h>
#include <stdio.h> // For perror

// Define global static variables for the RealProcess's state
static GPid g_real_process_pid = 0;
static gint g_real_process_in_fd = -1;  // Parent's write end to child's stdin
static gint g_real_process_out_fd = -1; // Parent's read end from child's stdout
static gint g_real_process_err_fd = -1; // Parent's read end from child's stderr

static GlobalProcessCallback g_real_process_out_cb = NULL;
static gpointer g_real_process_out_user_data = NULL;
static GlobalProcessCallback g_real_process_err_cb = NULL;
static gpointer g_real_process_err_user_data = NULL;

static GThread *g_real_process_out_thread = NULL;
static GThread *g_real_process_err_thread = NULL;
static gchar **g_real_process_argv = NULL;
static gboolean g_real_process_started = FALSE;

// Thread function to read stdout
static gpointer stdout_thread_global(gpointer /*data*/) {
  g_debug("real_process_global: stdout_thread_global starting");
  char buf[256];
  ssize_t n = 0; // Initialize n to fix potential uninitialized use warning
  while (g_real_process_out_fd >= 0 && (n = sys_read(g_real_process_out_fd, buf, sizeof(buf))) > 0) {
    if (g_real_process_out_cb) {
      GString *s = g_string_new_len(buf, n);
      g_real_process_out_cb(s, g_real_process_out_user_data); // Callback owns the GString
      // g_string_free(s, TRUE); // Caller of callback should free if needed, or cb copies.
                                // The original ProcessCallback implies data is transient.
                                // Let's assume callback copies if it needs to persist beyond its scope.
                                // For safety, let's ensure GString is freed by callback or here.
                                // The original code: p->out_cb(s, p->out_user); g_string_free(s, TRUE);
                                // So, we should free it after the call.
      g_string_free(s, TRUE);
    }
  }
  g_debug("real_process_global: stdout_thread_global exiting, n=%zd, errno=%d", n, n == -1 ? errno : 0);
  // close(g_real_process_out_fd); // Closing FD here might be premature if main process wants to check it
  // g_real_process_out_fd = -1;
  return NULL;
}

// Thread function to read stderr
static gpointer stderr_thread_global(gpointer /*data*/) {
  g_debug("real_process_global: stderr_thread_global starting");
  char buf[256];
  ssize_t n = 0; // Initialize n to fix potential uninitialized use warning
  while (g_real_process_err_fd >=0 && (n = sys_read(g_real_process_err_fd, buf, sizeof(buf))) > 0) {
    if (g_real_process_err_cb) {
      GString *s = g_string_new_len(buf, n);
      g_real_process_err_cb(s, g_real_process_err_user_data);
      g_string_free(s, TRUE);
    }
  }
  g_debug("real_process_global: stderr_thread_global exiting, n=%zd, errno=%d", n, n == -1 ? errno : 0);
  // close(g_real_process_err_fd);
  // g_real_process_err_fd = -1;
  return NULL;
}

// g_spawn_async_with_pipes child_setup function
static void child_setup_global(gpointer /*user_data*/) {
  g_debug("real_process_global: child_setup_global");
  setsid(); // Create a new session and set process group ID.
  // Ensure child dies if parent dies.
  if (prctl(PR_SET_PDEATHSIG, SIGKILL) < 0) {
    perror("prctl(PR_SET_PDEATHSIG, SIGKILL) failed");
    // Not a fatal error for the child's main operation, but good to note.
  }
}

// Initialize global process state from argv
void real_process_init_globals_from_argv(const gchar *const *argv) {
  g_debug("real_process_init_globals_from_argv: cmd=%s", argv && argv[0] ? argv[0] : "(null)");
  if (g_real_process_started) {
    g_warning("real_process_init_globals_from_argv: Process already initialized or started. Cleaning up old one.");
    real_process_cleanup_globals();
  }

  g_strfreev(g_real_process_argv);
  g_real_process_argv = g_strdupv((gchar**)argv);

  g_real_process_pid = 0;
  g_real_process_in_fd = -1;
  g_real_process_out_fd = -1;
  g_real_process_err_fd = -1;
  g_real_process_out_cb = NULL;
  g_real_process_out_user_data = NULL;
  g_real_process_err_cb = NULL;
  g_real_process_err_user_data = NULL;
  g_real_process_out_thread = NULL;
  g_real_process_err_thread = NULL;
  g_real_process_started = FALSE; // Set to TRUE in _start()
}

// Initialize global process state from a single command
void real_process_init_globals(const gchar *cmd) {
  g_debug("real_process_init_globals: cmd=%s", cmd ? cmd : "(null)");
  if (!cmd) {
    g_warning("real_process_init_globals: cmd is NULL.");
    // Initialize with NULL argv to indicate no command, or handle error appropriately.
    real_process_init_globals_from_argv(NULL);
    return;
  }
  // g_shell_parse_argv might be better for complex commands, but for a single path, this is fine.
  const gchar *argv[] = { cmd, NULL };
  real_process_init_globals_from_argv(argv);
}

void real_process_global_set_stdout_cb(GlobalProcessCallback cb, gpointer user_data) {
  g_debug("real_process_global_set_stdout_cb");
  g_real_process_out_cb = cb;
  g_real_process_out_user_data = user_data;
  if (cb && g_real_process_started && !g_real_process_out_thread && g_real_process_out_fd >=0) {
    g_real_process_out_thread = g_thread_new("process-stdout", stdout_thread_global, NULL);
  }
}

void real_process_global_set_stderr_cb(GlobalProcessCallback cb, gpointer user_data) {
  g_debug("real_process_global_set_stderr_cb");
  g_real_process_err_cb = cb;
  g_real_process_err_user_data = user_data;
  if (cb && g_real_process_started && !g_real_process_err_thread && g_real_process_err_fd >=0) {
    g_real_process_err_thread = g_thread_new("process-stderr", stderr_thread_global, NULL);
  }
}

void real_process_global_start() {
  g_debug("real_process_global_start");
  if (g_real_process_started) {
    g_warning("real_process_global_start: Process already started.");
    return;
  }
  if (!g_real_process_argv || !g_real_process_argv[0]) {
    g_printerr("real_process_global_start: No command (argv) to start.\n");
    return;
  }

  GError *error = NULL;
  // G_SPAWN_SEARCH_PATH looks for the command in PATH.
  // G_SPAWN_DO_NOT_REAP_CHILD means we are responsible for the child process (e.g. waitpid or let it be a zombie until exit).
  // g_spawn_close_pid() will be used in cleanup.
  if (!g_spawn_async_with_pipes(NULL, // working directory (current)
                                g_real_process_argv,
                                NULL, // envp (current)
                                G_SPAWN_DO_NOT_REAP_CHILD | G_SPAWN_SEARCH_PATH,
                                child_setup_global, // function to run in child before exec
                                NULL, // user_data for child_setup
                                &g_real_process_pid,
                                &g_real_process_in_fd,  // child's stdin
                                &g_real_process_out_fd, // child's stdout
                                &g_real_process_err_fd, // child's stderr
                                &error)) {
    g_printerr("real_process_global_start: g_spawn_async_with_pipes failed: %s\n", error ? error->message : "Unknown error");
    g_clear_error(&error);
    // Cleanup partially initialized state if spawn fails
    g_strfreev(g_real_process_argv);
    g_real_process_argv = NULL;
    if(g_real_process_in_fd >=0) { close(g_real_process_in_fd); g_real_process_in_fd = -1; }
    if(g_real_process_out_fd >=0) { close(g_real_process_out_fd); g_real_process_out_fd = -1; }
    if(g_real_process_err_fd >=0) { close(g_real_process_err_fd); g_real_process_err_fd = -1; }
    return;
  }

  g_debug("real_process_global_start: Spawned PID %d, in_fd=%d, out_fd=%d, err_fd=%d",
          g_real_process_pid, g_real_process_in_fd, g_real_process_out_fd, g_real_process_err_fd);

  g_real_process_started = TRUE;

  if (g_real_process_out_cb && !g_real_process_out_thread && g_real_process_out_fd >=0) {
    g_real_process_out_thread = g_thread_new("process-stdout", stdout_thread_global, NULL);
  }
  if (g_real_process_err_cb && !g_real_process_err_thread && g_real_process_err_fd >=0) {
    g_real_process_err_thread = g_thread_new("process-stderr", stderr_thread_global, NULL);
  }
}

gboolean real_process_global_write(const gchar *data, gssize len) {
  if (!g_real_process_started || g_real_process_in_fd < 0) {
    g_warning("real_process_global_write: Process not started or input FD is invalid.");
    return FALSE;
  }
  if (len < 0) {
    len = strlen(data);
  }
  g_debug("real_process_global_write: Writing %zd bytes to process stdin (fd %d)", len, g_real_process_in_fd);
  ssize_t written = sys_write(g_real_process_in_fd, data, len);
  if (written == -1) {
      g_printerr("real_process_global_write: write error to fd %d: %s (errno %d)\n", g_real_process_in_fd, g_strerror(errno), errno);
  } else if (written < len) {
      g_warning("real_process_global_write: partial write to fd %d. Wrote %zd of %zd bytes.", g_real_process_in_fd, written, len);
  }
  return written == len;
}

void real_process_cleanup_globals() {
  g_debug("real_process_cleanup_globals: Cleaning up global process resources.");

  // Close parent's ends of pipes first. This can signal EOF to child if it's reading.
  if (g_real_process_in_fd >= 0) {
    g_debug("real_process_cleanup_globals: Closing in_fd %d", g_real_process_in_fd);
    close(g_real_process_in_fd);
    g_real_process_in_fd = -1;
  }

  // Signal threads to stop by closing their FDs (read will return 0 or -1)
  // Note: Threads might be stuck in read(). Closing FDs they are reading from is a way to wake them up.
  if (g_real_process_out_fd >= 0) {
    g_debug("real_process_cleanup_globals: Closing out_fd %d", g_real_process_out_fd);
    close(g_real_process_out_fd); // This should help stdout_thread_global to exit
    g_real_process_out_fd = -1;
  }
  if (g_real_process_err_fd >= 0) {
    g_debug("real_process_cleanup_globals: Closing err_fd %d", g_real_process_err_fd);
    close(g_real_process_err_fd); // This should help stderr_thread_global to exit
    g_real_process_err_fd = -1;
  }

  // Join threads
  if (g_real_process_out_thread) {
    g_debug("real_process_cleanup_globals: Joining stdout_thread");
    g_thread_join(g_real_process_out_thread);
    g_real_process_out_thread = NULL; // g_thread_join also unrefs
  }
  if (g_real_process_err_thread) {
    g_debug("real_process_cleanup_globals: Joining stderr_thread");
    g_thread_join(g_real_process_err_thread);
    g_real_process_err_thread = NULL;
  }

  // Terminate and reap child process if it's still running
  if (g_real_process_pid > 0) {
    g_debug("real_process_cleanup_globals: Closing PID %d", g_real_process_pid);
    // GError *error = NULL; // This was unused
    g_spawn_close_pid(g_real_process_pid);
    g_real_process_pid = 0;
  }

  g_strfreev(g_real_process_argv);
  g_real_process_argv = NULL;

  g_real_process_started = FALSE;
  g_real_process_out_cb = NULL;
  g_real_process_err_cb = NULL;
  g_real_process_out_user_data = NULL;
  g_real_process_err_user_data = NULL;

  g_debug("real_process_cleanup_globals: Cleanup complete.");
}
