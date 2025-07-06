#include "preferences.h"
#include <glib/gstdio.h> // For g_build_filename, g_mkdir_with_parents
#include <glib.h>       // For GKeyFile, g_free, g_strdup, etc.
#include <errno.h>      // For errno, EEXIST

// Global static variables for preferences
static gchar   *g_pref_filename = NULL;
static gchar   *g_pref_sdk = NULL;
static guint16  g_pref_swank_port = 4005; // Default Swank port

// Forward declaration for internal load function
static void preferences_load_globals_internal();

// Public function to initialize global preference settings
void preferences_init_globals(const gchar *config_dir) {
  g_debug("preferences_init_globals: config_dir=%s", config_dir ? config_dir : "(null)");
  g_free(g_pref_filename); // Free if previously set (e.g., re-init)
  if (config_dir) {
    g_pref_filename = g_build_filename(config_dir, "glide", "preferences.ini", NULL);
  } else {
    // Fallback if config_dir is NULL, though g_get_user_config_dir() should usually provide it
    g_warning("preferences_init_globals: config_dir is NULL. Preferences might not load/save correctly.");
    g_pref_filename = g_strdup("glide_preferences.ini"); // Local fallback
  }

  g_free(g_pref_sdk); // Clear any existing SDK path
  g_pref_sdk = NULL;  // Initialize to NULL
  g_pref_swank_port = 4005; // Reset to default

  preferences_load_globals_internal();
  g_debug("preferences_init_globals: loaded filename='%s', sdk='%s', port=%u",
          g_pref_filename ? g_pref_filename : "(null)",
          g_pref_sdk ? g_pref_sdk : "(null)",
          g_pref_swank_port);
}

// Internal function to load preferences from the file into global variables
static void preferences_load_globals_internal() {
  if (!g_pref_filename || !g_file_test(g_pref_filename, G_FILE_TEST_EXISTS)) {
    g_debug("preferences_load_globals_internal: No preferences file found at '%s' or filename is NULL. Using defaults.", g_pref_filename ? g_pref_filename : "(null)");
    return; // Keep defaults if file doesn't exist or filename is NULL
  }
  g_debug("preferences_load_globals_internal: Loading from '%s'", g_pref_filename);

  GKeyFile *key_file = g_key_file_new();
  GError *error = NULL;

  if (g_key_file_load_from_file(key_file, g_pref_filename, G_KEY_FILE_NONE, &error)) {
    gchar *loaded_sdk = g_key_file_get_string(key_file, "General", "sdk", NULL); // No error check here, check loaded_sdk
    if (loaded_sdk) {
      g_free(g_pref_sdk);
      g_pref_sdk = loaded_sdk; // g_key_file_get_string allocates new memory
      g_debug("preferences_load_globals_internal: Loaded sdk='%s'", g_pref_sdk);
    } else {
      g_debug("preferences_load_globals_internal: 'sdk' key not found or empty.");
    }

    // For integer, check error to distinguish 'not found' from 'invalid value'
    GError *port_error = NULL;
    gint loaded_port = g_key_file_get_integer(key_file, "General", "swank_port", &port_error);
    if (port_error == NULL) {
      g_pref_swank_port = (guint16)loaded_port;
      g_debug("preferences_load_globals_internal: Loaded swank_port=%u", g_pref_swank_port);
    } else {
      g_debug("preferences_load_globals_internal: 'swank_port' key not found or invalid: %s. Using default %u.", port_error->message, g_pref_swank_port);
      g_clear_error(&port_error); // Clear error, keep default
    }
  } else {
    g_warning("preferences_load_globals_internal: Failed to load preferences file '%s': %s. Using defaults.", g_pref_filename, error->message);
    g_clear_error(&error); // Clear error, keep defaults
  }

  g_key_file_free(key_file);
}

// Public function to save current global preference values to the file
void preferences_save_globals() {
  if (!g_pref_filename) {
    g_warning("preferences_save_globals: g_pref_filename is NULL. Cannot save.");
    return;
  }
  g_debug("preferences_save_globals: Saving to '%s'", g_pref_filename);

  char *dir = g_path_get_dirname(g_pref_filename);
  if (dir && g_mkdir_with_parents(dir, 0700) != 0 && errno != EEXIST) {
    g_printerr("Failed to create config directory '%s': %s\n", dir, g_strerror(errno));
    // Continue to try saving anyway, maybe the file itself is writable.
  }
  g_free(dir);

  GKeyFile *key_file = g_key_file_new();
  GError *error = NULL;

  if (g_pref_sdk) {
    g_key_file_set_string(key_file, "General", "sdk", g_pref_sdk);
  } else {
    // If SDK is NULL, we might want to remove it from the file or save an empty string.
    // GKeyFile handles this by not adding the key if value is NULL.
    // To explicitly remove, use g_key_file_remove_key(). For now, this is fine.
  }
  g_key_file_set_integer(key_file, "General", "swank_port", g_pref_swank_port);

  if (!g_key_file_save_to_file(key_file, g_pref_filename, &error)) {
    g_printerr("Failed to save preferences to '%s': %s\n", g_pref_filename, error->message);
    g_clear_error(&error);
  } else {
    g_debug("preferences_save_globals: Successfully saved to '%s'", g_pref_filename);
  }

  g_key_file_free(key_file);
}

// Getter for global SDK path
const gchar *preferences_get_sdk_global() {
  return g_pref_sdk;
}

// Setter for global SDK path
void preferences_set_sdk_global(const gchar *new_sdk) {
  g_debug("preferences_set_sdk_global: new_sdk='%s'", new_sdk ? new_sdk : "(null)");
  if (g_strcmp0(g_pref_sdk, new_sdk) != 0) {
    g_free(g_pref_sdk);
    g_pref_sdk = g_strdup(new_sdk); // strdup handles NULL new_sdk (returns NULL)
    preferences_save_globals();
    // Signals are removed as this is no longer a GObject.
    // Callers needing to react to changes must now do so explicitly after calling set.
  }
}

// Getter for global Swank port
guint16 preferences_get_swank_port_global() {
  return g_pref_swank_port;
}

// Setter for global Swank port
void preferences_set_swank_port_global(guint16 new_port) {
  g_debug("preferences_set_swank_port_global: new_port=%u", new_port);
  if (g_pref_swank_port != new_port) {
    g_pref_swank_port = new_port;
    preferences_save_globals();
  }
}

// Optional: A function to free global preference data at application exit
void preferences_cleanup_globals() {
  g_debug("preferences_cleanup_globals: Freeing global preference data.");
  g_free(g_pref_filename);
  g_pref_filename = NULL;
  g_free(g_pref_sdk);
  g_pref_sdk = NULL;
  // g_pref_swank_port is not dynamically allocated.
}
