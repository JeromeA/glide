#include "project.h"
#include "string_text_provider.h"
#include "analyser.h"
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <errno.h>
#include "syscalls.h"

struct _ProjectFile {
  ProjectFileState state;
  gchar *path;
  GtkTextBuffer *buffer; /* nullable */
  TextProvider *provider; /* owned */
  LispLexer *lexer; /* owned */
  LispParser *parser; /* owned */
};

struct _Project {
  GObject parent_instance;
  GPtrArray *files; /* ProjectFile* */
  guint next_scratch_id;
};

enum {
  FILE_LOADED,
  N_SIGNALS
};

static guint project_signals[N_SIGNALS];

static void project_finalize(GObject *obj);
ProjectFile *project_create_scratch(Project *self);

G_DEFINE_TYPE(Project, project, G_TYPE_OBJECT)

static void project_class_init(ProjectClass *klass) {
  project_signals[FILE_LOADED] = g_signal_new(
      "file-loaded",
      G_TYPE_FROM_CLASS(klass),
      G_SIGNAL_RUN_FIRST,
      0,
      NULL, NULL,
      g_cclosure_marshal_VOID__POINTER,
      G_TYPE_NONE,
      1,
      G_TYPE_POINTER);

  GObjectClass *obj = G_OBJECT_CLASS(klass);
  obj->finalize = project_finalize;
}

static void project_file_free(ProjectFile *file) {
  if (!file) return;
  if (file->parser) lisp_parser_free(file->parser);
  if (file->lexer) lisp_lexer_free(file->lexer);
  if (file->provider) g_object_unref(file->provider);
  if (file->buffer) g_object_unref(file->buffer);
  g_free(file->path);
  g_free(file);
}

static void project_init(Project *self) {
  self->files = g_ptr_array_new_with_free_func((GDestroyNotify)project_file_free);
  self->next_scratch_id = 0;
}

static void project_finalize(GObject *obj) {
  Project *self = GLIDE_PROJECT(obj);
  if (self->files)
    g_ptr_array_free(self->files, TRUE);
  G_OBJECT_CLASS(project_parent_class)->finalize(obj);
}

Project *project_new(void) {
  Project *self = g_object_new(PROJECT_TYPE, NULL);
  project_create_scratch(self);
  return self;
}

ProjectFile *project_add_file(Project *self, TextProvider *provider,
    GtkTextBuffer *buffer, const gchar *path, ProjectFileState state) {
  g_return_val_if_fail(GLIDE_IS_PROJECT(self), NULL);
  g_return_val_if_fail(GLIDE_IS_TEXT_PROVIDER(provider), NULL);

  ProjectFile *file = g_new0(ProjectFile, 1);
  file->state = state;
  file->provider = g_object_ref(provider);
  file->buffer = buffer ? g_object_ref(buffer) : NULL;
  file->lexer = lisp_lexer_new(file->provider);
  file->parser = lisp_parser_new();
  file->path = path ? g_strdup(path) : NULL;

  g_ptr_array_add(self->files, file);

  project_file_changed(self, file);

  return file;
}

ProjectFile *project_create_scratch(Project *self) {
  g_return_val_if_fail(GLIDE_IS_PROJECT(self), NULL);
  gchar name[12];
  g_snprintf(name, sizeof(name), "scratch%02u", self->next_scratch_id++);
  TextProvider *provider = string_text_provider_new("");
  ProjectFile *file = project_add_file(self, provider, NULL, name,
      PROJECT_FILE_SCRATCH);
  g_object_unref(provider);
  return file;
}

guint project_get_file_count(Project *self) {
  g_return_val_if_fail(GLIDE_IS_PROJECT(self), 0);
  return self->files->len;
}

ProjectFile *project_get_file(Project *self, guint index) {
  g_return_val_if_fail(GLIDE_IS_PROJECT(self), NULL);
  if (index >= self->files->len)
    return NULL;
  return g_ptr_array_index(self->files, index);
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
  g_return_if_fail(GLIDE_IS_TEXT_PROVIDER(provider));
  if (file->parser)
    lisp_parser_free(file->parser);
  if (file->lexer)
    lisp_lexer_free(file->lexer);
  if (file->provider)
    g_object_unref(file->provider);
  if (file->buffer)
    g_object_unref(file->buffer);
  file->provider = g_object_ref(provider);
  file->buffer = buffer ? g_object_ref(buffer) : NULL;
  file->lexer = lisp_lexer_new(file->provider);
  file->parser = lisp_parser_new();
  project_file_changed(NULL, file);
}

TextProvider *project_file_get_provider(ProjectFile *file) {
  g_return_val_if_fail(file != NULL, NULL);
  return file->provider;
}

void project_file_changed(Project * /*self*/, ProjectFile *file) {
  g_return_if_fail(file != NULL);
  if (!file->lexer || !file->parser)
    return;
  lisp_lexer_lex(file->lexer);
  GArray *tokens = lisp_lexer_get_tokens(file->lexer);
  lisp_parser_parse(file->parser, tokens);
  const LispAstNode *ast = lisp_parser_get_ast(file->parser);
  if (ast)
    analyse_ast((LispAstNode*)ast);
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

gboolean project_file_load(Project *self, ProjectFile *file) {
  g_return_val_if_fail(GLIDE_IS_PROJECT(self), FALSE);
  g_return_val_if_fail(file != NULL, FALSE);

  const gchar *path = project_file_get_path(file);
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
  g_object_unref(provider);
  project_file_set_state(file, PROJECT_FILE_LIVE);

  g_signal_emit(self, project_signals[FILE_LOADED], 0, file);

  g_free(content);
  return TRUE;
}
