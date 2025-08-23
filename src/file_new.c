#include <gtk/gtk.h>
#include "file_new.h"
#include "app.h"
#include "project.h"
#include "string_text_provider.h"
#include "lisp_source_notebook.h"
#include "lisp_source_view.h"

void file_new(GtkWidget */*widget*/, gpointer data) {
  App *app = data;
  GtkWidget *dialog = gtk_file_chooser_dialog_new("New File", NULL,
      GTK_FILE_CHOOSER_ACTION_SAVE,
      "_Cancel", GTK_RESPONSE_CANCEL,
      "_Create", GTK_RESPONSE_ACCEPT,
      NULL);
  if (gtk_dialog_run(GTK_DIALOG(dialog)) == GTK_RESPONSE_ACCEPT) {
    gchar *filename = gtk_file_chooser_get_filename(GTK_FILE_CHOOSER(dialog));
    g_file_set_contents(filename, "", 0, NULL);
    Project *project = app_get_project(app);
    TextProvider *provider = string_text_provider_new("");
    ProjectFile *file = project_add_file(project, provider, NULL, filename, PROJECT_FILE_LIVE);
    text_provider_unref(provider);
    if (file) {
      project_file_load(project, file);
      LispSourceView *view = lisp_source_notebook_get_current_view(app_get_notebook(app));
      if (view)
        app_connect_view(app, view);
    }
    g_free(filename);
  }
  gtk_widget_destroy(dialog);
}
