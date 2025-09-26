#include "project_file.h"
#include "project.h"
#include "node.h"
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <errno.h>
#include <glib-object.h>
#include "syscalls.h"
#include "util.h"

struct _ProjectFile {
  ProjectFileState state;
  gchar *path;
  GString *content; /* owned */
  GArray *tokens; /* owned, LispToken */
  Node *ast; /* owned */
  LispLexer *lexer; /* owned */
  LispParser *parser; /* owned */
  Project *project;
  GArray *errors; /* ProjectFileError */
};

static ProjectFile *project_file_create(Project *project, GString *content,
    const gchar *path, ProjectFileState state);
static void project_file_assign_content(ProjectFile *file, GString *content);
static void project_file_clear_tokens(ProjectFile *file);
void project_file_set_tokens(ProjectFile *file, GArray *tokens);
static void project_file_clear_ast(ProjectFile *file);
void project_file_set_ast(ProjectFile *file, Node *ast);

ProjectFile *project_file_new(Project *project, GString *content,
    const gchar *path, ProjectFileState state) {
  g_return_val_if_fail(project != NULL, NULL);
  g_return_val_if_fail(glide_is_ui_thread(), NULL);
  return project_file_create(project, content, path, state);
}

ProjectFile *project_file_new_virtual(GString *content) {
  return project_file_create(NULL, content, NULL, PROJECT_FILE_LIVE);
}

void project_file_free(ProjectFile *file) {
  if (!file) return;
  if (file->parser) lisp_parser_free(file->parser);
  if (file->lexer) lisp_lexer_free(file->lexer);
  project_file_clear_ast(file);
  project_file_clear_tokens(file);
  if (file->content)
    g_string_free(file->content, TRUE);
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

void project_file_set_content(ProjectFile *file, GString *content) {
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
  project_file_assign_content(file, content);
}

const GString *project_file_get_content(ProjectFile *file) {
  g_return_val_if_fail(file != NULL, NULL);
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
  ProjectFile *file = project_file_create(project, text, path,
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
    const gchar *path, ProjectFileState state) {
  ProjectFile *file = g_new0(ProjectFile, 1);
  file->project = project;
  file->state = state;
  file->path = path ? g_strdup(path) : NULL;
  file->errors = g_array_new(FALSE, FALSE, sizeof(ProjectFileError));
  project_file_assign_content(file, content);
  return file;
}

static void project_file_assign_content(ProjectFile *file, GString *content) {
  file->content = content ? content : g_string_new("");
  file->lexer = lisp_lexer_new();
  file->parser = lisp_parser_new();
  file->tokens = NULL;
  file->ast = NULL;
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

const GArray *project_file_get_errors(ProjectFile *file) {
  g_return_val_if_fail(file != NULL, NULL);
  return file->errors;
}
