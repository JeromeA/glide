#include <gtk/gtk.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <errno.h>
#include <string.h>
#include "file_save.h"
#include "project.h"
#include "document.h"
#include "editor_manager.h"

void file_save(EditorManager *manager, Document *document) {
  g_return_if_fail(document != NULL);

  const gchar *filename = document_get_path(document);
  if (!filename)
    return;

  const GString *content = document_get_content(document);
  const gchar *text = content ? content->str : "";
  gsize length = content ? content->len : 0;

  int fd = open(filename, O_WRONLY | O_CREAT | O_TRUNC, 0666);
  if (fd == -1) {
    g_printerr("Failed to open document for writing: %s (errno: %d)\n", filename, errno);
    return;
  }

  size_t to_write = length;
  size_t total_written = 0;
  while (total_written < to_write) {
    ssize_t written = write(fd, text + total_written, to_write - total_written);
    if (written == -1) {
      g_printerr("Error writing to document: %s (errno: %d)\n", filename, errno);
      close(fd);
      return;
    }
    total_written += written;
  }

  if (close(fd) == -1)
    g_printerr("Error closing document: %s (errno: %d)\n", filename, errno);

  GtkTextBuffer *buffer = manager ? editor_manager_get_buffer(manager, document) : NULL;
  if (buffer)
    gtk_text_buffer_set_modified(buffer, FALSE);
}

void file_save_all(EditorManager *manager, Project *project) {
  g_return_if_fail(project != NULL);

  guint count = project_get_document_count(project);
  for (guint i = 0; i < count; i++) {
    Document *document = project_get_document(project, i);
    GtkTextBuffer *buffer = manager ? editor_manager_get_buffer(manager, document) : NULL;
    if (buffer && gtk_text_buffer_get_modified(buffer))
      file_save(manager, document);
  }
}

