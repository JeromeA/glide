#include <gtk/gtk.h>
#include "file_new.h"
#include "app.h"
#include "project.h"
#include "string_text_provider.h"
#include "lisp_source_notebook.h"
#include "editor.h"
#include "project_file.h"
#include "asdf.h"
#include "file_utilities.h"

void file_new(GtkWidget */*widget*/, gpointer data) {
  App *app = data;
  GtkWidget *dialog = gtk_dialog_new_with_buttons("New File", NULL,
      GTK_DIALOG_MODAL,
      "_Cancel", GTK_RESPONSE_CANCEL,
      "_Create", GTK_RESPONSE_ACCEPT,
      NULL);
  GtkWidget *content = gtk_dialog_get_content_area(GTK_DIALOG(dialog));
  GtkWidget *entry = gtk_entry_new();
  gtk_container_add(GTK_CONTAINER(content), entry);
  gtk_entry_set_activates_default(GTK_ENTRY(entry), TRUE);
  gtk_dialog_set_default_response(GTK_DIALOG(dialog), GTK_RESPONSE_ACCEPT);
  gtk_widget_show_all(dialog);
  if (gtk_dialog_run(GTK_DIALOG(dialog)) == GTK_RESPONSE_ACCEPT) {
    const gchar *name = gtk_entry_get_text(GTK_ENTRY(entry));
    if (name && *name) {
      gchar *fname = ensure_lisp_extension(name);
      Project *project = app_get_project(app);
      const gchar *dir = project_get_path(project);
      gchar *path = g_build_filename(dir ? dir : ".", fname, NULL);
      g_file_set_contents(path, "", 0, NULL);
      TextProvider *provider = string_text_provider_new("");
      ProjectFile *file = project_add_file(project, provider, NULL, path, PROJECT_FILE_LIVE);
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
        asdf_add_component(asdf, comp);
        asdf_save(asdf, asdf_get_filename(asdf));
        app_update_project_view(app);
      }
      g_free(path);
      g_free(fname);
    }
  }
  gtk_widget_destroy(dialog);
}
