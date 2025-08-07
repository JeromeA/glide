
#include "preferences.h"
#include <glib/gstdio.h>
#include <errno.h>

/* Define the Preferences class structure */
struct _Preferences {
    GObject parent_instance;
    gchar   *filename;
    gchar   *sdk;
    guint16  swank_port;
};

/* Define the Preferences class */
struct _PreferencesClass {
    GObjectClass parent_class;
};

G_DEFINE_TYPE(Preferences, preferences, G_TYPE_OBJECT);

/* Signals enum */
enum {
    SDK_CHANGED,
    PREFERENCES_SIGNAL_COUNT
};

static guint preferences_signals[PREFERENCES_SIGNAL_COUNT] = { 0 };

static void preferences_finalize(GObject *object) {
    Preferences *self = GLIDE_PREFERENCES(object);
    g_free(self->filename);
    g_free(self->sdk);
    G_OBJECT_CLASS(preferences_parent_class)->finalize(object);
}

/* Internal class initialization */
static void preferences_class_init(PreferencesClass *klass) {
  preferences_signals[SDK_CHANGED] = g_signal_new(
      "sdk-changed",
      G_TYPE_FROM_CLASS(klass),
      G_SIGNAL_RUN_FIRST,
      0,
      NULL, NULL,
      g_cclosure_marshal_VOID__STRING,
      G_TYPE_NONE,
      1,
      G_TYPE_STRING);

  GObjectClass *object_class = G_OBJECT_CLASS(klass);
  object_class->finalize = preferences_finalize;
}

static void preferences_load(Preferences *self) {
  GKeyFile *key_file = g_key_file_new();
  if (g_key_file_load_from_file(key_file, self->filename, G_KEY_FILE_NONE, NULL)) {
    char *sdk = g_key_file_get_string(key_file, "General", "sdk", NULL);
    if (sdk) {
      preferences_set_sdk(self, sdk);
      g_free(sdk);
    }

    /* load swank port if present */
    gint port = g_key_file_get_integer(key_file, "General", "swank_port", NULL);
    if (port) {
      self->swank_port = (guint16)port;
    }
  }

  g_key_file_free(key_file);
}

static void preferences_save(Preferences *self) {
  /* Ensure that the configuration directory exists */
  char *dir = g_path_get_dirname(self->filename);
  if (g_mkdir_with_parents(dir, 0700) != 0 && errno != EEXIST) {
    g_printerr("Failed to create config directory '%s': %s\n", dir,
               g_strerror(errno));
  }
  g_free(dir);

  GKeyFile *key_file = g_key_file_new();
  GError *error = NULL;

  if (self->sdk)
    g_key_file_set_string(key_file, "General", "sdk", self->sdk);
  g_key_file_set_integer(key_file, "General", "swank_port", self->swank_port);

  if (!g_key_file_save_to_file(key_file, self->filename, &error)) {
    g_printerr("Failed to save config: %s\n", error->message);
    g_clear_error(&error);
  }

  g_key_file_free(key_file);
}

/* Internal instance initialization */
static void preferences_init(Preferences *self) {
  self->filename   = NULL;
  self->sdk        = NULL;
  self->swank_port = 4005;
}

Preferences *
preferences_new(const gchar *config_dir)
{
  Preferences *self = g_object_new(PREFERENCES_TYPE, NULL);
  self->filename = g_build_filename(config_dir, "glide", "preferences.ini", NULL);
  preferences_load(self);
  return self;
}

const gchar *preferences_get_sdk(Preferences *self) {
  return self->sdk;
}

void preferences_set_sdk(Preferences *self, const gchar *new_sdk) {
  if (g_strcmp0(self->sdk, new_sdk) != 0) {
    g_free(self->sdk);
    self->sdk = g_strdup(new_sdk);
    preferences_save(self);
    g_signal_emit(self, preferences_signals[SDK_CHANGED], 0, new_sdk);
  }
}

guint16 preferences_get_swank_port(Preferences *self) {
  return self->swank_port;
}

void preferences_set_swank_port(Preferences *self, guint16 new_port) {
  if (self->swank_port != new_port) {
    self->swank_port = new_port;
    preferences_save(self);
  }
}

