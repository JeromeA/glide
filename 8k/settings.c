
#include "settings.h"

/* Define the Settings class structure */
struct _Settings {
    GObject parent_instance;
    gchar *sdk;
};

/* Define the Settings class */
struct _SettingsClass {
    GObjectClass parent_class;
};

G_DEFINE_TYPE(Settings, settings, G_TYPE_OBJECT);

/* Signals enum */
enum {
    SDK_CHANGED,
    SIGNAL_COUNT
};

static guint settings_signals[SIGNAL_COUNT] = { 0 };

/* Static variable to hold the singleton instance */
static Settings *singleton_instance = NULL;

static void settings_finalize(GObject *object) {
    Settings *self = SETTINGS_CLASS(object);
    g_free(self->sdk);
    G_OBJECT_CLASS(settings_parent_class)->finalize(object);
}

/* Internal class initialization */
static void settings_class_init(SettingsClass *klass) {
  settings_signals[SDK_CHANGED] = g_signal_new(
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
  object_class->finalize = settings_finalize;
}

static char *settings_get_filename() {
  const char *config_dir = g_get_user_config_dir();
  return g_build_filename(config_dir, "glide", "settings.ini", NULL);
}

static void settings_load(Settings *self) {
  char *filename = settings_get_filename();
  GKeyFile *key_file = g_key_file_new();
  GError *error = NULL;
  if (g_key_file_load_from_file(key_file, filename, G_KEY_FILE_NONE, &error)) {
    char *sdk = g_key_file_get_string(key_file, "General", "sdk", &error);
    if (sdk) {
      settings_set_sdk(self, sdk);
      g_free(sdk);
    }
  }

  g_key_file_free(key_file);
  g_free(filename);
}

static void settings_save(Settings *self) {
  char *filename = settings_get_filename();

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
static void settings_init(Settings *self) {
  self->sdk = NULL;
  settings_load(self);
}

/* Public API: Get the singleton instance */
Settings *settings_get_instance(void) {
  if (!singleton_instance) {
    singleton_instance = g_object_new(SETTINGS_TYPE, NULL);
  }
  return singleton_instance;
}

const gchar *settings_get_sdk(Settings *self) {
  return self->sdk;
}

void settings_set_sdk(Settings *self, const gchar *new_sdk) {
  if (g_strcmp0(self->sdk, new_sdk) != 0) {
    g_free(self->sdk);
    self->sdk = g_strdup(new_sdk);
    settings_save(self);
    g_signal_emit(self, settings_signals[SDK_CHANGED], 0, new_sdk);
  }
}

