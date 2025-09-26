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
#include "editor_manager.h"

void file_save(EditorManager *manager, ProjectFile *file) {
  g_return_if_fail(file != NULL);

  const gchar *filename = project_file_get_path(file);
  if (!filename)
    return;

  const GString *content = project_file_get_content(file);
  const gchar *text = content ? content->str : "";
  gsize length = content ? content->len : 0;

  int fd = sys_open(filename, O_WRONLY | O_CREAT | O_TRUNC, 0666);
  if (fd == -1) {
    g_printerr("Failed to open file for writing: %s (errno: %d)\n", filename, errno);
    return;
  }

  size_t to_write = length;
  size_t total_written = 0;
  while (total_written < to_write) {
    ssize_t written = sys_write(fd, text + total_written, to_write - total_written);
    if (written == -1) {
      g_printerr("Error writing to file: %s (errno: %d)\n", filename, errno);
      sys_close(fd);
      return;
    }
    total_written += written;
  }

  if (sys_close(fd) == -1)
    g_printerr("Error closing file: %s (errno: %d)\n", filename, errno);

  GtkTextBuffer *buffer = manager ? editor_manager_get_buffer(manager, file) : NULL;
  if (buffer)
    gtk_text_buffer_set_modified(buffer, FALSE);
}

void file_save_all(EditorManager *manager, Project *project) {
  g_return_if_fail(project != NULL);

  guint count = project_get_file_count(project);
  for (guint i = 0; i < count; i++) {
    ProjectFile *file = project_get_file(project, i);
    GtkTextBuffer *buffer = manager ? editor_manager_get_buffer(manager, file) : NULL;
    if (buffer && gtk_text_buffer_get_modified(buffer))
      file_save(manager, file);
  }
}

