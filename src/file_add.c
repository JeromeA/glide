#include <gtk/gtk.h>
#include "file_add.h"
#include "app.h"
#include "project.h"
#include "string_text_provider.h"
#include "lisp_source_notebook.h"
#include "editor.h"
#include "project_file.h"
#include "asdf.h"

void file_add(GtkWidget */*widget*/, gpointer data) {
  App *app = data;
  GtkWidget *dialog = gtk_file_chooser_dialog_new("Add File",
      NULL,
      GTK_FILE_CHOOSER_ACTION_OPEN,
      "_Cancel", GTK_RESPONSE_CANCEL,
      "_Add", GTK_RESPONSE_ACCEPT,
      NULL);
  Project *project = app_get_project(app);
  const gchar *dir = project_get_path(project);
  if (dir)
    gtk_file_chooser_set_current_folder(GTK_FILE_CHOOSER(dialog), dir);
  if (gtk_dialog_run(GTK_DIALOG(dialog)) == GTK_RESPONSE_ACCEPT) {
    gchar *filename = gtk_file_chooser_get_filename(GTK_FILE_CHOOSER(dialog));
    TextProvider *provider = string_text_provider_new("");
    ProjectFile *file = project_add_file(project, provider, NULL, filename, PROJECT_FILE_LIVE);
    text_provider_unref(provider);
    if (file) {
      project_file_load(file);
      Editor *view = lisp_source_notebook_get_current_editor(app_get_notebook(app));
      if (view)
        app_connect_editor(app, view);
      Asdf *asdf = project_get_asdf(project);
      const gchar *rel = project_file_get_relative_path(file);
      GString *comp = g_string_new(rel);
      if (g_str_has_suffix(comp->str, ".lisp"))
        g_string_truncate(comp, comp->len - 5);
      asdf_add_component(asdf, comp->str);
      asdf_save(asdf, asdf_get_filename(asdf));
      app_update_project_view(app);
      g_string_free(comp, TRUE);
    }
    g_free(filename);
  }
  gtk_widget_destroy(dialog);
}

