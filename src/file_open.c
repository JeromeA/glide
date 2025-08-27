#include <gtk/gtk.h>
#include <gtksourceview/gtksource.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <errno.h>
#include "reloc.h"
#include "syscalls.h"
#include "file_open.h"
#include "app.h"
#include "editor.h"
#include "lisp_source_notebook.h"
#include "project.h"
#include "string_text_provider.h"
#include "file_save.h"
#include "preferences.h"
#include "asdf.h"
#include "file_utilities.h"

static gboolean save_if_modified(App *app) {
  Project *project = app_get_project(app);
  if (project_get_file_count(project) == 0)
    return TRUE;
  ProjectFile *file = project_get_file(project, 0);
  GtkTextBuffer *buffer = project_file_get_buffer(file);
  if (buffer && gtk_text_buffer_get_modified(buffer)) {
    GtkWidget *dialog = gtk_message_dialog_new(NULL, GTK_DIALOG_MODAL,
        GTK_MESSAGE_QUESTION, GTK_BUTTONS_NONE,
        "Save changes to %s?", project_file_get_path(file));
    gtk_dialog_add_button(GTK_DIALOG(dialog), "_Cancel", GTK_RESPONSE_CANCEL);
    gtk_dialog_add_button(GTK_DIALOG(dialog), "_Discard", GTK_RESPONSE_REJECT);
    gtk_dialog_add_button(GTK_DIALOG(dialog), "_Save", GTK_RESPONSE_ACCEPT);
    gint res = gtk_dialog_run(GTK_DIALOG(dialog));
    gtk_widget_destroy(dialog);
    if (res == GTK_RESPONSE_CANCEL)
      return FALSE;
    if (res == GTK_RESPONSE_ACCEPT)
      file_save(file);
  }
  return TRUE;
}

gboolean file_open_path(App *app, const gchar *filename) {
  g_debug("file_open_path %s", filename);
  g_return_val_if_fail(app != NULL, FALSE);
  if (!save_if_modified(app))
    return FALSE;

  Project *project = app_get_project(app);
  LispSourceNotebook *notebook = app_get_notebook(app);
  project_clear(project);

  gboolean is_asdf = g_str_has_suffix(filename, ".asd");
  if (is_asdf) {
    Asdf *asdf = asdf_new_from_file(filename);
    project_set_asdf(project, asdf);
    g_object_unref(asdf);
    gchar *base = g_path_get_dirname(filename);
    project_set_path(project, base);
    gchar *dir = g_strdup(base);
    for (guint i = 0; i < asdf_get_component_count(project_get_asdf(project)); i++) {
      const gchar *comp = asdf_get_component(project_get_asdf(project), i);
      gchar *path = g_build_filename(dir, comp, NULL);
      gchar *full = ensure_lisp_extension(path);
      g_free(path);
      TextProvider *provider = string_text_provider_new("");
      ProjectFile *pf = project_add_file(project, provider, NULL, full, PROJECT_FILE_LIVE);
      text_provider_unref(provider);
      if (pf)
        project_file_load(pf);
      g_free(full);
    }
    g_free(dir);
    g_free(base);
  } else {
    project_set_asdf(project, NULL);
    gchar *dir = g_path_get_dirname(filename);
    project_set_path(project, dir);
    TextProvider *provider = string_text_provider_new("");
    ProjectFile *file = project_add_file(project, provider, NULL, filename, PROJECT_FILE_LIVE);
    text_provider_unref(provider);
    if (file)
      project_file_load(file);
    g_free(dir);
  }

  Preferences *prefs = app_get_preferences(app);
  preferences_set_project_file(prefs, filename);
  preferences_add_recent_project(prefs, filename);
  app_update_recent_menu(app);

  Editor *view = lisp_source_notebook_get_current_editor(notebook);
  if (view)
    app_connect_editor(app, view);
  app_update_asdf_view(app);
  return TRUE;
}

void file_open(GtkWidget */*widget*/, gpointer data) {
  App *app = (App *)data;

  GtkWidget *dialog = gtk_file_chooser_dialog_new(
      "Open File",
      NULL,
      GTK_FILE_CHOOSER_ACTION_OPEN,
      "_Cancel", GTK_RESPONSE_CANCEL,
      "_Open", GTK_RESPONSE_ACCEPT,
      NULL);

  if (gtk_dialog_run(GTK_DIALOG(dialog)) == GTK_RESPONSE_ACCEPT) {
    gchar *filename = gtk_file_chooser_get_filename(GTK_FILE_CHOOSER(dialog));
    file_open_path(app, filename);
    g_free(filename);
  }
  gtk_widget_destroy(dialog);
}
