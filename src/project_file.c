#include "project_file.h"
#include "project.h"
#include "string_text_provider.h"
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <errno.h>
#include <glib-object.h>
#include "syscalls.h"

struct _ProjectFile {
  ProjectFileState state;
  gchar *path;
  GtkTextBuffer *buffer; /* nullable */
  TextProvider *provider; /* owned */
  LispLexer *lexer; /* owned */
  LispParser *parser; /* owned */
  Project *project;
};

ProjectFile *project_file_new(Project *project, TextProvider *provider,
    GtkTextBuffer *buffer, const gchar *path, ProjectFileState state) {
  g_return_val_if_fail(project != NULL, NULL);
  g_return_val_if_fail(provider, NULL);

  ProjectFile *file = g_new0(ProjectFile, 1);
  file->project = project;
  file->state = state;
  file->provider = text_provider_ref(provider);
  file->buffer = buffer ? g_object_ref(buffer) : NULL;
  file->lexer = lisp_lexer_new(file->provider);
  file->parser = lisp_parser_new();
  file->path = path ? g_strdup(path) : NULL;
  return file;
}

void project_file_free(ProjectFile *file) {
  if (!file) return;
  if (file->parser) lisp_parser_free(file->parser);
  if (file->lexer) lisp_lexer_free(file->lexer);
  if (file->provider) text_provider_unref(file->provider);
  if (file->buffer) g_object_unref(file->buffer);
  g_free(file->path);
  g_free(file);
}

ProjectFileState project_file_get_state(ProjectFile *file) {
  g_return_val_if_fail(file != NULL, PROJECT_FILE_DORMANT);
  return file->state;
}

void project_file_set_state(ProjectFile *file, ProjectFileState state) {
  g_return_if_fail(file != NULL);
  file->state = state;
}

void project_file_set_provider(ProjectFile *file, TextProvider *provider,
    GtkTextBuffer *buffer) {
  g_return_if_fail(file != NULL);
  g_return_if_fail(provider);
  if (file->parser)
    lisp_parser_free(file->parser);
  if (file->lexer)
    lisp_lexer_free(file->lexer);
  if (file->provider)
    text_provider_unref(file->provider);
  if (file->buffer)
    g_object_unref(file->buffer);
  file->provider = text_provider_ref(provider);
  file->buffer = buffer ? g_object_ref(buffer) : NULL;
  file->lexer = lisp_lexer_new(file->provider);
  file->parser = lisp_parser_new();
}

TextProvider *project_file_get_provider(ProjectFile *file) {
  g_return_val_if_fail(file != NULL, NULL);
  return file->provider;
}

LispParser *project_file_get_parser(ProjectFile *file) {
  g_return_val_if_fail(file != NULL, NULL);
  return file->parser;
}

LispLexer *project_file_get_lexer(ProjectFile *file) {
  g_return_val_if_fail(file != NULL, NULL);
  return file->lexer;
}

GtkTextBuffer *project_file_get_buffer(ProjectFile *file) {
  g_return_val_if_fail(file != NULL, NULL);
  return file->buffer;
}

const gchar *project_file_get_path(ProjectFile *file) {
  g_return_val_if_fail(file != NULL, NULL);
  return file->path;
}

void project_file_set_path(ProjectFile *file, const gchar *path) {
  g_return_if_fail(file != NULL);
  g_free(file->path);
  file->path = path ? g_strdup(path) : NULL;
}

gboolean project_file_load(ProjectFile *file) {
  g_return_val_if_fail(file != NULL, FALSE);

  const gchar *path = project_file_get_path(file);
  g_debug("project_file_load path=%s", path ? path : "(null)");
  if (!path)
    return FALSE;

  int fd = sys_open(path, O_RDONLY, 0);
  if (fd == -1) {
    g_printerr("Failed to open file using syscalls: %s (errno: %d)\n", path, errno);
    return FALSE;
  }

  struct stat sb;
  if (sys_fstat(fd, &sb) == -1 || !S_ISREG(sb.st_mode)) {
    g_printerr("Not a regular file: %s\n", path);
    sys_close(fd);
    return FALSE;
  }

  off_t length = sb.st_size;
  char *content = g_malloc(length + 1);
  if (!content) {
    g_printerr("Failed to allocate memory for file content.\n");
    sys_close(fd);
    return FALSE;
  }

  ssize_t total_read = 0;
  while (total_read < length) {
    ssize_t r = sys_read(fd, content + total_read, length - total_read);
    if (r == -1) {
      g_printerr("Error reading file: %s (errno: %d)\n", path, errno);
      g_free(content);
      sys_close(fd);
      return FALSE;
    } else if (r == 0) {
      break;
    }
    total_read += r;
  }

  content[total_read] = '\0';
  sys_close(fd);

  TextProvider *provider = string_text_provider_new(content);
  project_file_set_provider(file, provider, NULL);
  text_provider_unref(provider);
  project_file_set_state(file, PROJECT_FILE_LIVE);

  Project *self = file->project;
  if (self)
    project_file_loaded(self, file);

  g_free(content);
  return TRUE;
}

const gchar *project_file_get_relative_path(ProjectFile *file) {
  g_return_val_if_fail(file != NULL, NULL);
  const gchar *path = file->path;
  if (!path)
    return NULL;
  Project *project = file->project;
  const gchar *project_path = project ? project_get_path(project) : NULL;
  if (project_path && g_str_has_prefix(path, project_path)) {
    const gchar *rel = path + strlen(project_path);
    if (g_str_has_prefix(rel, "/")) rel++;
    return rel;
  }
  return path;
}
