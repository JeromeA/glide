#include "document.h"
#include "project.h"
#include "node.h"
#include "lisp_lexer.h"
#include "lisp_parser.h"
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <errno.h>
#include <glib-object.h>
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

static void document_clear_tokens(Document *document);
static void document_clear_ast(Document *document);
static void document_set_tokens(Document *document, GArray *tokens);
static void document_set_ast(Document *document, Node *ast);

Document *document_new(Project *project, DocumentState state) {
  if (project)
    g_return_val_if_fail(glide_is_ui_thread(), NULL);

  Document *document = g_new0(Document, 1);
  document->project = project;
  document->state = state;
  document->path = NULL;
  document->errors = g_array_new(FALSE, FALSE, sizeof(DocumentError));
  document->content = NULL;
  document->tokens = NULL;
  document->ast = NULL;
  return document;
}

void document_free(Document *document) {
  g_return_if_fail(document != NULL);
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
  if (document->project)
    g_return_if_fail(glide_is_ui_thread());
  if (document->content) {
    g_string_free(document->content, TRUE);
    document->content = NULL;
  }
  document->content = content ? content : g_string_new("");
  document_reparse(document);
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

void document_reparse(Document *document) {
  g_return_if_fail(document != NULL);
  if (document->project)
    g_return_if_fail(glide_is_ui_thread());

  document_clear_errors(document);
  document_clear_ast(document);
  document_clear_tokens(document);

  const GString *content = document->content;
  if (content) {
    GArray *tokens = lisp_lexer_lex(content);
    document_set_tokens(document, tokens);

    Node *ast = lisp_parser_parse(tokens, document);
    document_set_ast(document, ast);
  }

  if (document->project)
    project_document_changed(document->project, document);
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

GString *document_load_buffer(const gchar *path) {
  g_return_val_if_fail(path != NULL, NULL);
  g_return_val_if_fail(glide_is_ui_thread(), NULL);
  LOG(1, "document_load_buffer path=%s", path);

  int fd = open(path, O_RDONLY, 0);
  if (fd == -1) {
    g_printerr("Failed to open file using syscalls: %s (errno: %d)\n", path, errno);
    return NULL;
  }

  struct stat sb;
  if (fstat(fd, &sb) == -1 || !S_ISREG(sb.st_mode)) {
    g_printerr("Not a regular file: %s\n", path);
    close(fd);
    return NULL;
  }

  off_t length = sb.st_size;
  char *content = g_malloc(length + 1);
  if (!content) {
    g_printerr("Failed to allocate memory for file content.\n");
    close(fd);
    return NULL;
  }

  ssize_t total_read = 0;
  while (total_read < length) {
    ssize_t r = read(fd, content + total_read, length - total_read);
    if (r == -1) {
      g_printerr("Error reading file: %s (errno: %d)\n", path, errno);
      g_free(content);
      close(fd);
      return NULL;
    } else if (r == 0) {
      break;
    }
    total_read += r;
  }

  content[total_read] = '\0';
  close(fd);

  GString *text = g_string_new_len(content, total_read);
  g_free(content);

  return text;
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

void document_add_error(Document *document, DocumentError error) {
  g_return_if_fail(document != NULL);
  if (!document->errors)
    document->errors = g_array_new(FALSE, FALSE, sizeof(DocumentError));
  g_return_if_fail(error.end > error.start);
  const gchar *path = document->path ? document->path : "(null)";
  LOG(1, "document_add_error path=%s range=[%zu,%zu) type=%d message=%s",
      path, error.start, error.end, error.type,
      error.message ? error.message : "(null)");
  DocumentError stored = error;
  stored.message = error.message ? g_strdup(error.message) : NULL;
  g_array_append_val(document->errors, stored);
}

static void document_clear_tokens(Document *document) {
  g_return_if_fail(document != NULL);
  if (!document->tokens) return;
  g_array_free(document->tokens, TRUE);
  document->tokens = NULL;
}

static void document_set_tokens(Document *document, GArray *tokens) {
  g_return_if_fail(document != NULL);
  document_clear_tokens(document);
  document->tokens = tokens;
}

static void document_clear_ast(Document *document) {
  g_return_if_fail(document != NULL);
  if (!document->ast) return;
  node_free_deep(document->ast);
  document->ast = NULL;
}

static void document_set_ast(Document *document, Node *ast) {
  g_return_if_fail(document != NULL);
  document_clear_ast(document);
  document->ast = ast;
}

const GArray *document_get_errors(Document *document) {
  g_return_val_if_fail(document != NULL, NULL);
  return document->errors;
}
