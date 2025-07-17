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

    int fd = sys_open(filename, O_RDONLY, 0);
    if (fd == -1) {
      g_printerr("Failed to open file using syscalls: %s (errno: %d)\n", filename, errno);
    } else {
      struct stat sb;
      if (sys_fstat(fd, &sb) == -1) {
        g_printerr("Failed to stat file: %s (errno: %d)\n", filename, errno);
        sys_close(fd);
      } else if (!S_ISREG(sb.st_mode)) {
        g_printerr("Not a regular file: %s\n", filename);
        sys_close(fd);
      } else {
        off_t length = sb.st_size;
        char *content = g_malloc(length + 1);
        if (!content) {
          g_printerr("Failed to allocate memory for file content.\n");
          sys_close(fd);
        } else {
          ssize_t total_read = 0;
          while (total_read < length) {
            ssize_t r = sys_read(fd, content + total_read, length - total_read);
            if (r == -1) {
              g_printerr("Error reading file: %s (errno: %d)\n", filename, errno);
              break;
            } else if (r == 0) {
              break;
            }
            total_read += r;
          }
          content[total_read] = '\0';
          sys_close(fd);

          TextProvider *provider = string_text_provider_new(content);
          ProjectFile *file = project_add_file(project, provider, NULL, filename, PROJECT_FILE_LIVE);
          g_object_unref(provider);

          gint page = lisp_source_notebook_add_file(notebook, file);
          gtk_notebook_set_current_page(GTK_NOTEBOOK(notebook), page);
          LispSourceView *view = lisp_source_notebook_get_current_view(notebook);
          app_connect_view(app, view);

          g_free(content);
        }
      }
    }

    g_free(filename);
  }

  // Destroy the dialog
  gtk_widget_destroy(dialog);
}
