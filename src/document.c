#include "document.h"
#include "project.h"
#include "node.h"
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <errno.h>
#include <glib-object.h>
#include "syscalls.h"
#include "util.h"

struct _Document {
  DocumentState state;
  gchar *path;
  GString *content; /* owned */
  GArray *tokens; /* owned, LispToken */
  Node *ast; /* owned */
  Project *project;
  GArray *errors; /* DocumentError */
};

static Document *document_create(Project *project, GString *content,
    const gchar *path, DocumentState state);
static void document_assign_content(Document *document, GString *content);
static void document_clear_tokens(Document *document);
void document_set_tokens(Document *document, GArray *tokens);
static void document_clear_ast(Document *document);
void document_set_ast(Document *document, Node *ast);

Document *document_new(Project *project, GString *content,
    const gchar *path, DocumentState state) {
  g_return_val_if_fail(project != NULL, NULL);
  g_return_val_if_fail(glide_is_ui_thread(), NULL);
  return document_create(project, content, path, state);
}

Document *document_new_virtual(GString *content) {
  return document_create(NULL, content, NULL, DOCUMENT_LIVE);
}

void document_free(Document *document) {
  if (!document) return;
  document_clear_ast(document);
  document_clear_tokens(document);
  if (document->content)
    g_string_free(document->content, TRUE);
  if (document->errors) {
    document_clear_errors(document);
    g_array_free(document->errors, TRUE);
  }
  g_free(document->path);
  g_free(document);
}

DocumentState document_get_state(Document *document) {
  g_return_val_if_fail(document != NULL, DOCUMENT_DORMANT);
  return document->state;
}

void document_set_state(Document *document, DocumentState state) {
  g_return_if_fail(document != NULL);
  g_return_if_fail(glide_is_ui_thread());
  document->state = state;
}

void document_set_content(Document *document, GString *content) {
  g_return_if_fail(document != NULL);
  g_return_if_fail(glide_is_ui_thread());
  document_clear_errors(document);
  document_clear_ast(document);
  document_clear_tokens(document);
  if (document->content) {
    g_string_free(document->content, TRUE);
    document->content = NULL;
  }
  document_assign_content(document, content);
  if (document->project)
    project_document_changed(document->project, document);
}

const GString *document_get_content(Document *document) {
  g_return_val_if_fail(document != NULL, NULL);
  return document->content;
}

const GArray *document_get_tokens(Document *document) {
  g_return_val_if_fail(document != NULL, NULL);
  return document->tokens;
}

const Node *document_get_ast(Document *document) {
  g_return_val_if_fail(document != NULL, NULL);
  return document->ast;
}

const gchar *document_get_path(Document *document) {
  g_return_val_if_fail(document != NULL, NULL);
  return document->path;
}

void document_set_path(Document *document, const gchar *path) {
  g_return_if_fail(document != NULL);
  g_return_if_fail(glide_is_ui_thread());
  g_free(document->path);
  document->path = path ? g_strdup(path) : NULL;
}

Document *document_load(Project *project, const gchar *path) {
  g_return_val_if_fail(project != NULL, NULL);
  g_return_val_if_fail(path != NULL, NULL);
  g_return_val_if_fail(glide_is_ui_thread(), NULL);

  LOG(1, "document_load path=%s", path);

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
  Document *document = document_create(project, text, path,
      DOCUMENT_LIVE);
  g_free(content);

  return document;
}

const gchar *document_get_relative_path(Document *document) {
  g_return_val_if_fail(document != NULL, NULL);
  const gchar *path = document->path;
  if (!path)
    return NULL;
  Project *project = document->project;
  const gchar *project_path = project ? project_get_path(project) : NULL;
  if (project_path && g_str_has_prefix(path, project_path)) {
    const gchar *rel = path + strlen(project_path);
    if (g_str_has_prefix(rel, "/")) rel++;
    return rel;
  }
  return path;
}

void document_clear_errors(Document *document) {
  g_return_if_fail(document != NULL);
  const gchar *path = document->path ? document->path : "(null)";
  guint count = document->errors ? document->errors->len : 0;
  LOG(1, "document_clear_errors path=%s count=%u", path, count);
  if (document->errors) {
    for (guint i = 0; i < document->errors->len; i++) {
      DocumentError *err = &g_array_index(document->errors, DocumentError, i);
      g_free(err->message);
      err->message = NULL;
    }
    g_array_set_size(document->errors, 0);
  }
}

void document_add_error(Document *document, gsize start, gsize end,
    const gchar *message) {
  g_return_if_fail(document != NULL);
  if (!document->errors)
    document->errors = g_array_new(FALSE, FALSE, sizeof(DocumentError));
  if (end <= start)
    return;
  const gchar *path = document->path ? document->path : "(null)";
  LOG(1, "document_add_error path=%s range=[%zu,%zu) message=%s", path, start,
      end, message ? message : "(null)");
  DocumentError err = { start, end, message ? g_strdup(message) : NULL };
  g_array_append_val(document->errors, err);
}

static Document *document_create(Project *project, GString *content,
    const gchar *path, DocumentState state) {
  Document *document = g_new0(Document, 1);
  document->project = project;
  document->state = state;
  document->path = path ? g_strdup(path) : NULL;
  document->errors = g_array_new(FALSE, FALSE, sizeof(DocumentError));
  document_assign_content(document, content);
  return document;
}

static void document_assign_content(Document *document, GString *content) {
  document->content = content ? content : g_string_new("");
  document->tokens = NULL;
  document->ast = NULL;
}

static void document_clear_tokens(Document *document) {
  if (!document || !document->tokens)
    return;
  g_array_free(document->tokens, TRUE);
  document->tokens = NULL;
}

void document_set_tokens(Document *document, GArray *tokens) {
  if (!document)
    return;
  document_clear_tokens(document);
  document->tokens = tokens;
}

static void document_clear_ast(Document *document) {
  if (!document || !document->ast)
    return;
  node_free_deep(document->ast);
  document->ast = NULL;
}

void document_set_ast(Document *document, Node *ast) {
  if (!document)
    return;
  document_clear_ast(document);
  document->ast = ast;
}

const GArray *document_get_errors(Document *document) {
  g_return_val_if_fail(document != NULL, NULL);
  return document->errors;
}
