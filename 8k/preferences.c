
#include "preferences.h"

/* Define the Preferences class structure */
struct _Preferences {
    GObject parent_instance;
    gchar *sdk;
};

/* Define the Preferences class */
struct _PreferencesClass {
    GObjectClass parent_class;
};

G_DEFINE_TYPE(Preferences, preferences, G_TYPE_OBJECT);

/* Signals enum */
enum {
    SDK_CHANGED,
    SIGNAL_COUNT
};

static guint preferences_signals[SIGNAL_COUNT] = { 0 };

/* Static variable to hold the singleton instance */
static Preferences *singleton_instance = NULL;

static void preferences_finalize(GObject *object) {
    Preferences *self = PREFERENCES_CLASS(object);
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

static char *preferences_get_filename() {
  const char *config_dir = g_get_user_config_dir();
  return g_build_filename(config_dir, "glide", "preferences.ini", NULL);
}

static void preferences_load(Preferences *self) {
  char *filename = preferences_get_filename();
  GKeyFile *key_file = g_key_file_new();
  GError *error = NULL;
  if (g_key_file_load_from_file(key_file, filename, G_KEY_FILE_NONE, &error)) {
    char *sdk = g_key_file_get_string(key_file, "General", "sdk", &error);
    if (sdk) {
      preferences_set_sdk(self, sdk);
      g_free(sdk);
    }
  }

  g_key_file_free(key_file);
  g_free(filename);
}

static void preferences_save(Preferences *self) {
  char *filename = preferences_get_filename();

  GKeyFile *key_file = g_key_file_new();
  GError *error = NULL;

  g_key_file_set_string(key_file, "General", "sdk", self->sdk);

  if (!g_key_file_save_to_file(key_file, filename, &error)) {
    g_printerr("Failed to save config: %s\n", error->message);
    g_clear_error(&error);
  }

  g_key_file_free(key_file);
  g_free(filename);
}

/* Internal instance initialization */
static void preferences_init(Preferences *self) {
  self->sdk = NULL;
  preferences_load(self);
}

/* Public API: Get the singleton instance */
Preferences *preferences_get_instance(void) {
  if (!singleton_instance) {
    singleton_instance = g_object_new(PREFERENCES_TYPE, NULL);
  }
  return singleton_instance;
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

