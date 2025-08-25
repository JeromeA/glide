#include <gtk/gtk.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <errno.h>
#include <string.h>
#include "reloc.h"
#include "syscalls.h"
#include "file_save.h"
#include "project.h"
#include "project_file.h"

void file_save(ProjectFile *file) {
  g_return_if_fail(file != NULL);

  const gchar *filename = project_file_get_path(file);
  if (!filename)
    return;

  GtkTextBuffer *buffer = project_file_get_buffer(file);
  g_return_if_fail(buffer != NULL);

  GtkTextIter start, end;
  gtk_text_buffer_get_start_iter(buffer, &start);
  gtk_text_buffer_get_end_iter(buffer, &end);

  gchar *buffer_text = gtk_text_buffer_get_text(buffer, &start, &end, FALSE);

  int fd = sys_open(filename, O_WRONLY | O_CREAT | O_TRUNC, 0666);
  if (fd == -1) {
    g_printerr("Failed to open file for writing: %s (errno: %d)\n", filename, errno);
    g_free(buffer_text);
    return;
  }

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

  if (sys_close(fd) == -1)
    g_printerr("Error closing file: %s (errno: %d)\n", filename, errno);

  gtk_text_buffer_set_modified(buffer, FALSE);
  g_free(buffer_text);
}

void file_save_all(Project *project) {
  g_return_if_fail(project != NULL);

  guint count = project_get_file_count(project);
  for (guint i = 0; i < count; i++) {
    ProjectFile *file = project_get_file(project, i);
    GtkTextBuffer *buffer = project_file_get_buffer(file);
    if (buffer && gtk_text_buffer_get_modified(buffer))
      file_save(file);
  }
}

