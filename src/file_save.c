#include <gtk/gtk.h>
#include <gtksourceview/gtksource.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <errno.h>
#include "reloc.h"
#include "syscalls.h"
#include "file_save.h"
#include "app.h"
#include "lisp_source_view.h"

void file_save(GtkWidget *, gpointer data) {
  App *app = (App *) data;
  GtkSourceBuffer *source_buffer =
      lisp_source_view_get_buffer(app_get_source_view(app));
  ProjectFile *file = lisp_source_view_get_file(app_get_source_view(app));
  const gchar *filename = project_file_get_path(file);
  gboolean scratch = project_file_get_state(file) == PROJECT_FILE_SCRATCH;

  gchar *chosen_filename = NULL;

  // Check if we already have a filename
  if (!filename || scratch) {
    // We do not have a known filename -> use a "Save As" dialog
    GtkWidget *dialog = gtk_file_chooser_dialog_new(
        "Save File",
        NULL,
        GTK_FILE_CHOOSER_ACTION_SAVE,
        "_Cancel", GTK_RESPONSE_CANCEL,
        "_Save", GTK_RESPONSE_ACCEPT,
        NULL
        );

    // Suggest a name or remember the last directory, etc.
    gtk_file_chooser_set_do_overwrite_confirmation(GTK_FILE_CHOOSER(dialog), TRUE);

    if (gtk_dialog_run(GTK_DIALOG(dialog)) == GTK_RESPONSE_ACCEPT) {
      chosen_filename = gtk_file_chooser_get_filename(GTK_FILE_CHOOSER(dialog));
      project_file_set_path(file, chosen_filename);
      project_file_set_state(file, PROJECT_FILE_LIVE);
      filename = chosen_filename;
    }

    gtk_widget_destroy(dialog);

    // If the user canceled, filename is still NULL
    if (!filename) {
      return; // bail out
    }
  }

  // At this point, we have a valid filename (either from opening a file or from Save As).
  // Get the text from the buffer
  GtkTextIter start, end;
  gtk_text_buffer_get_start_iter(GTK_TEXT_BUFFER(source_buffer), &start);
  gtk_text_buffer_get_end_iter(GTK_TEXT_BUFFER(source_buffer), &end);

  gchar *buffer_text = gtk_text_buffer_get_text(GTK_TEXT_BUFFER(source_buffer),
      &start, &end, FALSE);

  // Open (or create/truncate) the file for writing using syscalls
  int fd = sys_open(filename, O_WRONLY | O_CREAT | O_TRUNC, 0666);
  if (fd == -1) {
    g_printerr("Failed to open file for writing: %s (errno: %d)\n", filename, errno);
    g_free(buffer_text);
    return;
  }

  // Write the text to the file using a loop to handle partial writes
  size_t to_write = strlen(buffer_text);
  size_t total_written = 0;
  while (total_written < to_write) {
    ssize_t written = sys_write(fd, buffer_text + total_written, to_write - total_written);
    if (written == -1) {
      g_printerr("Error writing to file: %s (errno: %d)\n", filename, errno);
      sys_close(fd);
      g_free(buffer_text);
      return;
    }
    total_written += written;
  }

  // Close the file
  if (sys_close(fd) == -1) {
    g_printerr("Error closing file: %s (errno: %d)\n", filename, errno);
  }

  g_free(buffer_text);
  g_free(chosen_filename);
}

void file_saveas(GtkWidget *widget, gpointer data) {
  App *app = (App *) data;
  ProjectFile *file = lisp_source_view_get_file(app_get_source_view(app));
  gchar *old_filename = g_strdup(project_file_get_path(file));
  project_file_set_path(file, NULL);

  file_save(widget, app);

  const char *new_filename = project_file_get_path(file);
  if (!new_filename) {
    project_file_set_path(file, old_filename);
  }
  g_free(old_filename);
}

void file_save_all(GtkWidget * /*widget*/, gpointer data) {
  App *app = (App *) data;
  LispSourceNotebook *notebook = app_get_notebook(app);
  if (!notebook)
    return;
  gint current = gtk_notebook_get_current_page(GTK_NOTEBOOK(notebook));
  gint pages = gtk_notebook_get_n_pages(GTK_NOTEBOOK(notebook));
  for (gint i = 0; i < pages; i++) {
    GtkWidget *child = gtk_notebook_get_nth_page(GTK_NOTEBOOK(notebook), i);
    GtkTextBuffer *buffer = GTK_TEXT_BUFFER(lisp_source_view_get_buffer(LISP_SOURCE_VIEW(child)));
    if (buffer && gtk_text_buffer_get_modified(buffer)) {
      gtk_notebook_set_current_page(GTK_NOTEBOOK(notebook), i);
      file_save(NULL, app);
    }
  }
  gtk_notebook_set_current_page(GTK_NOTEBOOK(notebook), current);
}
