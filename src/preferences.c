
#include "preferences.h"
#include <glib/gstdio.h>
#include <errno.h>

struct _Preferences {
  gchar   *filename;
  gchar   *sdk;
  guint16  swank_port;
  gchar   *project_file;
  gint     refcnt;
};

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

    char *proj = g_key_file_get_string(key_file, "General", "project_file", NULL);
    if (proj) {
      preferences_set_project_file(self, proj);
      g_free(proj);
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
  if (self->project_file)
    g_key_file_set_string(key_file, "General", "project_file", self->project_file);

  if (!g_key_file_save_to_file(key_file, self->filename, &error)) {
    g_printerr("Failed to save config: %s\n", error->message);
    g_clear_error(&error);
  }

  g_key_file_free(key_file);
}

/* Internal instance initialization */
static void preferences_free(Preferences *self) {
  g_free(self->filename);
  g_free(self->sdk);
  g_free(self->project_file);
  g_free(self);
}

Preferences *
preferences_new(const gchar *config_dir)
{
  Preferences *self = g_new0(Preferences, 1);
  self->swank_port = 4005;
  self->refcnt = 1;
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

const gchar *preferences_get_project_file(Preferences *self) {
  return self->project_file;
}

void preferences_set_project_file(Preferences *self, const gchar *file) {
  if (g_strcmp0(self->project_file, file) != 0) {
    g_free(self->project_file);
    self->project_file = g_strdup(file);
    preferences_save(self);
  }
}

Preferences *preferences_ref(Preferences *self) {
  g_return_val_if_fail(self != NULL, NULL);
  g_atomic_int_inc(&self->refcnt);
  return self;
}

void preferences_unref(Preferences *self) {
  if (!self)
    return;
  if (g_atomic_int_dec_and_test(&self->refcnt))
    preferences_free(self);
}

