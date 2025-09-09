#include <gtk/gtk.h>
#include <glib/gstdio.h>
#include "file_delete.h"
#include "app.h"
#include "project.h"
#include "project_file.h"
#include "asdf.h"

void file_delete(GtkWidget */*widget*/, gpointer data) {
  App *app = data;
  ProjectFile *file = app_get_current_file(app);
  if (!file)
    return;
  const gchar *path = project_file_get_path(file);
  if (!path)
    return;
  gchar *basename = g_path_get_basename(path);
  GtkWidget *dialog = gtk_message_dialog_new(NULL, 0,
      GTK_MESSAGE_QUESTION, GTK_BUTTONS_NONE,
      "Delete file '%s' and all its references", basename);
  gtk_dialog_add_buttons(GTK_DIALOG(dialog),
      "_Cancel", GTK_RESPONSE_CANCEL,
      "_Delete", GTK_RESPONSE_ACCEPT,
      NULL);
  if (gtk_dialog_run(GTK_DIALOG(dialog)) == GTK_RESPONSE_ACCEPT) {
    const gchar *rel = project_file_get_relative_path(file);
    gchar *comp = NULL;
    if (rel) {
      comp = g_path_get_basename(rel);
      gchar *dot = g_strrstr(comp, ".");
      if (dot)
        *dot = '\0';
    }
    g_remove(path);
    Asdf *asdf = project_get_asdf(app_get_project(app));
    if (comp) {
      GString *comp_str = g_string_new(comp);
      asdf_remove_component(asdf, comp_str);
      g_string_free(comp_str, TRUE);
      asdf_save(asdf, asdf_get_filename(asdf));
      app_update_project_view(app);
    }
    Project *project = app_get_project(app);
    project_remove_file(project, file);
    g_free(comp);
  }
  gtk_widget_destroy(dialog);
  g_free(basename);
}
