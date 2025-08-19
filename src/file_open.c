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
#include "lisp_source_view.h"
#include "lisp_source_notebook.h"
#include "project.h"
#include "string_text_provider.h"

void file_open(GtkWidget *, gpointer data) {
  App *app = (App *) data;
  Project *project = app_get_project(app);
  LispSourceNotebook *notebook = app_get_notebook(app);

  // Create a file chooser dialog
  GtkWidget *dialog = gtk_file_chooser_dialog_new(
      "Open File",
      NULL,
      GTK_FILE_CHOOSER_ACTION_OPEN,
      "_Cancel", GTK_RESPONSE_CANCEL,
      "_Open",   GTK_RESPONSE_ACCEPT,
      NULL);

  if (gtk_dialog_run(GTK_DIALOG(dialog)) == GTK_RESPONSE_ACCEPT) {
    gchar* filename = gtk_file_chooser_get_filename(GTK_FILE_CHOOSER(dialog));

    TextProvider *provider = string_text_provider_new("");
    ProjectFile *file = project_add_file(project, provider, NULL, filename, PROJECT_FILE_LIVE);
    text_provider_unref(provider);

    if (file) {
      project_file_load(project, file);
      LispSourceView *view = lisp_source_notebook_get_current_view(notebook);
      app_connect_view(app, view);
    }

    g_free(filename);
  }

  // Destroy the dialog
  gtk_widget_destroy(dialog);
}
