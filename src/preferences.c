
#include "preferences.h"
#include <glib/gstdio.h>
#include <errno.h>
#include "util.h"

struct _Preferences {
  gchar   *filename;
  gchar   *sdk;
  gchar   *project_file;
  gchar   *project_dir;
  gint     window_width;
  gint     window_height;
  gint     project_view_width;
  gchar   *last_file;
  gint     cursor_position;
  GList   *recent_projects;
  gboolean auto_save;
  gint     refcnt;
};

static void preferences_load(Preferences *self) {
  LOG(1, "preferences_load %s", self->filename);
  self->auto_save = FALSE;
  GKeyFile *key_file = g_key_file_new();
  if (g_key_file_load_from_file(key_file, self->filename, G_KEY_FILE_NONE, NULL)) {
    char *sdk = g_key_file_get_string(key_file, "General", "sdk", NULL);
    if (sdk) {
      preferences_set_sdk(self, sdk);
      g_free(sdk);
    }


    char *proj = g_key_file_get_string(key_file, "General", "project_file", NULL);
    if (proj) {
      LOG(1, "preferences_load project_file %s", proj);
      preferences_set_project_file(self, proj);
      g_free(proj);
    }

    char *proj_dir = g_key_file_get_string(key_file, "General", "project_dir", NULL);
    if (proj_dir) {
      preferences_set_project_dir(self, proj_dir);
      g_free(proj_dir);
    }

    gint win_w = g_key_file_get_integer(key_file, "General", "window_width", NULL);
    if (win_w)
      preferences_set_window_width(self, win_w);

    gint win_h = g_key_file_get_integer(key_file, "General", "window_height", NULL);
    if (win_h)
      preferences_set_window_height(self, win_h);

    gint width = g_key_file_get_integer(key_file, "General", "project_view_width", NULL);
    if (width)
      preferences_set_project_view_width(self, width);

    char *last = g_key_file_get_string(key_file, "General", "last_file", NULL);
    if (last) {
      preferences_set_last_file(self, last);
      g_free(last);
    }

    gint pos = g_key_file_get_integer(key_file, "General", "cursor_position", NULL);
    if (pos)
      preferences_set_cursor_position(self, pos);

    gsize len = 0;
    char **recent = g_key_file_get_string_list(key_file, "General", "recent_projects", &len, NULL);
    for (gsize i = 0; i < len && i < 5; i++)
      self->recent_projects = g_list_append(self->recent_projects, g_strdup(recent[i]));
    g_strfreev(recent);
  }

  g_key_file_free(key_file);
  self->auto_save = TRUE;
}

static void preferences_save(Preferences *self) {
  if (!self->auto_save)
    return;
  LOG(1, "preferences_save project_file=%s", self->project_file);
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
  if (self->project_file)
    g_key_file_set_string(key_file, "General", "project_file", self->project_file);
  if (self->project_dir)
    g_key_file_set_string(key_file, "General", "project_dir", self->project_dir);
  g_key_file_set_integer(key_file, "General", "window_width", self->window_width);
  g_key_file_set_integer(key_file, "General", "window_height", self->window_height);
  g_key_file_set_integer(key_file, "General", "project_view_width", self->project_view_width);
  if (self->last_file)
    g_key_file_set_string(key_file, "General", "last_file", self->last_file);
  g_key_file_set_integer(key_file, "General", "cursor_position", self->cursor_position);

  gsize len = g_list_length(self->recent_projects);
  if (len) {
    const gchar **arr = g_new(const gchar *, len);
    GList *l = self->recent_projects;
    for (gsize i = 0; i < len; i++, l = l->next)
      arr[i] = l->data;
    g_key_file_set_string_list(key_file, "General", "recent_projects", arr, len);
    g_free(arr);
  } else {
    g_key_file_remove_key(key_file, "General", "recent_projects", NULL);
  }

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
  g_free(self->project_dir);
  g_free(self->last_file);
  g_list_free_full(self->recent_projects, g_free);
  g_free(self);
}

Preferences *
preferences_new(const gchar *config_dir)
{
  Preferences *self = g_new0(Preferences, 1);
  self->refcnt = 1;
  self->filename = g_build_filename(config_dir, "glide", "preferences.ini", NULL);
  self->project_dir = g_strdup("~/lisp");
  self->window_width = 800;
  self->window_height = 600;
  self->project_view_width = 200;
  self->cursor_position = 0;
  self->auto_save = TRUE;
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

const gchar *preferences_get_project_file(Preferences *self) {
  return self->project_file;
}

void preferences_set_project_file(Preferences *self, const gchar *file) {
  LOG(1, "preferences_set_project_file %s", file);
  if (g_strcmp0(self->project_file, file) != 0) {
    g_free(self->project_file);
    self->project_file = g_strdup(file);
    preferences_save(self);
  }
}

const gchar *preferences_get_project_dir(Preferences *self) {
  return self->project_dir;
}

void preferences_set_project_dir(Preferences *self, const gchar *dir) {
  if (g_strcmp0(self->project_dir, dir) != 0) {
    g_free(self->project_dir);
    self->project_dir = g_strdup(dir);
    preferences_save(self);
  }
}

gint preferences_get_project_view_width(Preferences *self) {
  return self->project_view_width;
}

void preferences_set_project_view_width(Preferences *self, gint width) {
  if (self->project_view_width != width) {
    self->project_view_width = width;
    preferences_save(self);
  }
}

gint preferences_get_window_width(Preferences *self) {
  return self->window_width;
}

void preferences_set_window_width(Preferences *self, gint width) {
  if (self->window_width != width) {
    self->window_width = width;
    preferences_save(self);
  }
}

gint preferences_get_window_height(Preferences *self) {
  return self->window_height;
}

void preferences_set_window_height(Preferences *self, gint height) {
  if (self->window_height != height) {
    self->window_height = height;
    preferences_save(self);
  }
}

const gchar *preferences_get_last_file(Preferences *self) {
  return self->last_file;
}

void preferences_set_last_file(Preferences *self, const gchar *file) {
  LOG(1, "preferences_set_last_file %s", file);
  if (g_strcmp0(self->last_file, file) != 0) {
    g_free(self->last_file);
    self->last_file = g_strdup(file);
    preferences_save(self);
  }
}

gint preferences_get_cursor_position(Preferences *self) {
  return self->cursor_position;
}

void preferences_set_cursor_position(Preferences *self, gint pos) {
  if (self->cursor_position != pos) {
    self->cursor_position = pos;
    preferences_save(self);
  }
}

const GList *preferences_get_recent_projects(Preferences *self) {
  return self->recent_projects;
}

void preferences_add_recent_project(Preferences *self, const gchar *path) {
  GList *link = g_list_find_custom(self->recent_projects, path, (GCompareFunc)g_strcmp0);
  if (link) {
    g_free(link->data);
    self->recent_projects = g_list_delete_link(self->recent_projects, link);
  }
  self->recent_projects = g_list_prepend(self->recent_projects, g_strdup(path));
  while (g_list_length(self->recent_projects) > 5) {
    GList *last = g_list_last(self->recent_projects);
    g_free(last->data);
    self->recent_projects = g_list_delete_link(self->recent_projects, last);
  }
  preferences_save(self);
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

