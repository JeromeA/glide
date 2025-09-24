#include "project_file.h"
#include "project.h"
#include "node.h"
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <errno.h>
#include <glib-object.h>
#include <gtk/gtk.h>
#include "syscalls.h"
#include "util.h"

struct _ProjectFile {
  ProjectFileState state;
  gchar *path;
  GtkTextBuffer *buffer; /* nullable */
  GString *content; /* owned */
  GArray *tokens; /* owned, LispToken */
  Node *ast; /* owned */
  LispLexer *lexer; /* owned */
  LispParser *parser; /* owned */
  Project *project;
  GtkTextTag *error_tag;
  GArray *errors; /* ProjectFileError */
};

static void project_file_ensure_error_tag(ProjectFile *file);
static ProjectFile *project_file_create(Project *project, GString *content,
    GtkTextBuffer *buffer, const gchar *path, ProjectFileState state);
static void project_file_assign_content(ProjectFile *file, GString *content,
    GtkTextBuffer *buffer);
static void project_file_clear_tokens(ProjectFile *file);
void project_file_set_tokens(ProjectFile *file, GArray *tokens);
static void project_file_clear_ast(ProjectFile *file);
void project_file_set_ast(ProjectFile *file, Node *ast);
static void project_file_refresh_content(ProjectFile *file);

ProjectFile *project_file_new(Project *project, GString *content,
    GtkTextBuffer *buffer, const gchar *path, ProjectFileState state) {
  g_return_val_if_fail(project != NULL, NULL);
  g_return_val_if_fail(glide_is_ui_thread(), NULL);
  return project_file_create(project, content, buffer, path, state);
}

ProjectFile *project_file_new_virtual(GString *content) {
  return project_file_create(NULL, content, NULL, NULL, PROJECT_FILE_LIVE);
}

void project_file_free(ProjectFile *file) {
  if (!file) return;
  if (file->parser) lisp_parser_free(file->parser);
  if (file->lexer) lisp_lexer_free(file->lexer);
  project_file_clear_ast(file);
  project_file_clear_tokens(file);
  if (file->content)
    g_string_free(file->content, TRUE);
  if (file->buffer) g_object_unref(file->buffer);
  if (file->errors) {
    project_file_clear_errors(file);
    g_array_free(file->errors, TRUE);
  }
  g_free(file->path);
  g_free(file);
}

ProjectFileState project_file_get_state(ProjectFile *file) {
  g_return_val_if_fail(file != NULL, PROJECT_FILE_DORMANT);
  return file->state;
}

void project_file_set_state(ProjectFile *file, ProjectFileState state) {
  g_return_if_fail(file != NULL);
  g_return_if_fail(glide_is_ui_thread());
  file->state = state;
}

void project_file_set_content(ProjectFile *file, GString *content,
    GtkTextBuffer *buffer) {
  g_return_if_fail(file != NULL);
  g_return_if_fail(glide_is_ui_thread());
  project_file_clear_errors(file);
  project_file_clear_ast(file);
  project_file_clear_tokens(file);
  if (file->parser) {
    lisp_parser_free(file->parser);
    file->parser = NULL;
  }
  if (file->lexer) {
    lisp_lexer_free(file->lexer);
    file->lexer = NULL;
  }
  if (file->content) {
    g_string_free(file->content, TRUE);
    file->content = NULL;
  }
  if (file->buffer) {
    g_object_unref(file->buffer);
    file->buffer = NULL;
  }
  project_file_assign_content(file, content, buffer);
}

const GString *project_file_get_content(ProjectFile *file) {
  g_return_val_if_fail(file != NULL, NULL);
  project_file_refresh_content(file);
  return file->content;
}

const GArray *project_file_get_tokens(ProjectFile *file) {
  g_return_val_if_fail(file != NULL, NULL);
  return file->tokens;
}

const Node *project_file_get_ast(ProjectFile *file) {
  g_return_val_if_fail(file != NULL, NULL);
  return file->ast;
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
  g_return_if_fail(glide_is_ui_thread());
  g_free(file->path);
  file->path = path ? g_strdup(path) : NULL;
}

ProjectFile *project_file_load(Project *project, const gchar *path) {
  g_return_val_if_fail(project != NULL, NULL);
  g_return_val_if_fail(path != NULL, NULL);
  g_return_val_if_fail(glide_is_ui_thread(), NULL);

  LOG(1, "project_file_load path=%s", path);

  int fd = sys_open(path, O_RDONLY, 0);
  if (fd == -1) {
    g_printerr("Failed to open file using syscalls: %s (errno: %d)\n", path, errno);
    return NULL;
  }

  struct stat sb;
  if (sys_fstat(fd, &sb) == -1 || !S_ISREG(sb.st_mode)) {
    g_printerr("Not a regular file: %s\n", path);
    sys_close(fd);
    return NULL;
  }

  off_t length = sb.st_size;
  char *content = g_malloc(length + 1);
  if (!content) {
    g_printerr("Failed to allocate memory for file content.\n");
    sys_close(fd);
    return NULL;
  }

  ssize_t total_read = 0;
  while (total_read < length) {
    ssize_t r = sys_read(fd, content + total_read, length - total_read);
    if (r == -1) {
      g_printerr("Error reading file: %s (errno: %d)\n", path, errno);
      g_free(content);
      sys_close(fd);
      return NULL;
    } else if (r == 0) {
      break;
    }
    total_read += r;
  }

  content[total_read] = '\0';
  sys_close(fd);

  GString *text = g_string_new_len(content, total_read);
  ProjectFile *file = project_file_create(project, text, NULL, path,
      PROJECT_FILE_LIVE);
  g_free(content);

  return file;
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

void project_file_clear_errors(ProjectFile *file) {
  g_return_if_fail(file != NULL);
  const gchar *path = file->path ? file->path : "(null)";
  guint count = file->errors ? file->errors->len : 0;
  LOG(1, "project_file_clear_errors path=%s count=%u", path, count);
  if (file->errors) {
    for (guint i = 0; i < file->errors->len; i++) {
      ProjectFileError *err = &g_array_index(file->errors, ProjectFileError, i);
      g_free(err->message);
      err->message = NULL;
    }
    g_array_set_size(file->errors, 0);
  }
  if (file->buffer && file->error_tag) {
    GtkTextIter start;
    GtkTextIter end;
    gtk_text_buffer_get_bounds(file->buffer, &start, &end);
    gtk_text_buffer_remove_tag(file->buffer, file->error_tag, &start, &end);
  }
}

void project_file_add_error(ProjectFile *file, gsize start, gsize end,
    const gchar *message) {
  g_return_if_fail(file != NULL);
  if (!file->errors)
    file->errors = g_array_new(FALSE, FALSE, sizeof(ProjectFileError));
  if (end <= start)
    return;
  const gchar *path = file->path ? file->path : "(null)";
  LOG(1, "project_file_add_error path=%s range=[%zu,%zu) message=%s", path, start,
      end, message ? message : "(null)");
  ProjectFileError err = { start, end, message ? g_strdup(message) : NULL };
  g_array_append_val(file->errors, err);
}

static ProjectFile *project_file_create(Project *project, GString *content,
    GtkTextBuffer *buffer, const gchar *path, ProjectFileState state) {
  ProjectFile *file = g_new0(ProjectFile, 1);
  file->project = project;
  file->state = state;
  file->path = path ? g_strdup(path) : NULL;
  file->error_tag = NULL;
  file->errors = g_array_new(FALSE, FALSE, sizeof(ProjectFileError));
  project_file_assign_content(file, content, buffer);
  return file;
}

static void project_file_assign_content(ProjectFile *file, GString *content,
    GtkTextBuffer *buffer) {
  file->content = content ? content : g_string_new("");
  file->buffer = buffer ? g_object_ref(buffer) : NULL;
  file->lexer = lisp_lexer_new();
  file->parser = lisp_parser_new();
  file->tokens = NULL;
  file->ast = NULL;
  file->error_tag = NULL;
}

static void project_file_clear_tokens(ProjectFile *file) {
  if (!file || !file->tokens)
    return;
  g_array_free(file->tokens, TRUE);
  file->tokens = NULL;
}

void project_file_set_tokens(ProjectFile *file, GArray *tokens) {
  if (!file)
    return;
  project_file_clear_tokens(file);
  file->tokens = tokens;
}

static void project_file_clear_ast(ProjectFile *file) {
  if (!file || !file->ast)
    return;
  node_free_deep(file->ast);
  file->ast = NULL;
}

void project_file_set_ast(ProjectFile *file, Node *ast) {
  if (!file)
    return;
  project_file_clear_ast(file);
  file->ast = ast;
}

static void project_file_refresh_content(ProjectFile *file) {
  if (!file || !file->buffer)
    return;
  GtkTextIter start;
  GtkTextIter end;
  gtk_text_buffer_get_start_iter(file->buffer, &start);
  gtk_text_buffer_get_end_iter(file->buffer, &end);
  gchar *text = gtk_text_buffer_get_text(file->buffer, &start, &end, FALSE);
  if (!file->content)
    file->content = g_string_new(text ? text : "");
  else
    g_string_assign(file->content, text ? text : "");
  g_free(text);
}

static void project_file_ensure_error_tag(ProjectFile *file) {
  if (!file->buffer)
    return;
  if (!file->error_tag) {
    file->error_tag = gtk_text_buffer_create_tag(file->buffer, NULL,
        "underline", PANGO_UNDERLINE_ERROR, NULL);
  }
}

void project_file_apply_errors(ProjectFile *file) {
  g_return_if_fail(file != NULL);
  if (!file->buffer || !file->errors || file->errors->len == 0)
    return;
  const gchar *path = file->path ? file->path : "(null)";
  LOG(1, "project_file_apply_errors path=%s count=%u", path, file->errors->len);
  project_file_ensure_error_tag(file);
  if (!file->error_tag)
    return;
  for (guint i = 0; i < file->errors->len; i++) {
    ProjectFileError *err = &g_array_index(file->errors, ProjectFileError, i);
    LOG(1, "project_file_apply_errors applying range=[%zu,%zu) message=%s",
        err->start, err->end, err->message ? err->message : "(null)");
    GtkTextIter start;
    GtkTextIter end;
    gtk_text_buffer_get_iter_at_offset(file->buffer, &start, (gint)err->start);
    gtk_text_buffer_get_iter_at_offset(file->buffer, &end, (gint)err->end);
    gtk_text_buffer_apply_tag(file->buffer, file->error_tag, &start, &end);
  }
}

const GArray *project_file_get_errors(ProjectFile *file) {
  g_return_val_if_fail(file != NULL, NULL);
  return file->errors;
}
