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
  GHashTable *function_defs; /* name -> GPtrArray* NodeInfo* */
  GHashTable *function_uses;
  GHashTable *variable_defs;
  GHashTable *variable_uses;
  GHashTable *package_defs;
  GHashTable *package_uses;
};

enum {
  FILE_LOADED,
  N_SIGNALS
};

static guint project_signals[N_SIGNALS];

static void project_finalize(GObject *obj);
ProjectFile *project_create_scratch(Project *self);
static void project_index_clear(Project *self);
static void project_index_node(Project *self, const LispAstNode *node);
static void project_index_walk(Project *self, const LispAstNode *node);
static GHashTable *project_index_table(Project *self, NodeInfoKind kind);

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
  self->function_defs = g_hash_table_new_full(g_str_hash, g_str_equal, g_free, (GDestroyNotify)g_ptr_array_unref);
  self->function_uses = g_hash_table_new_full(g_str_hash, g_str_equal, g_free, (GDestroyNotify)g_ptr_array_unref);
  self->variable_defs = g_hash_table_new_full(g_str_hash, g_str_equal, g_free, (GDestroyNotify)g_ptr_array_unref);
  self->variable_uses = g_hash_table_new_full(g_str_hash, g_str_equal, g_free, (GDestroyNotify)g_ptr_array_unref);
  self->package_defs = g_hash_table_new_full(g_str_hash, g_str_equal, g_free, (GDestroyNotify)g_ptr_array_unref);
  self->package_uses = g_hash_table_new_full(g_str_hash, g_str_equal, g_free, (GDestroyNotify)g_ptr_array_unref);
}

static void project_finalize(GObject *obj) {
  Project *self = GLIDE_PROJECT(obj);
  project_index_clear(self);
  g_clear_pointer(&self->function_defs, g_hash_table_unref);
  g_clear_pointer(&self->function_uses, g_hash_table_unref);
  g_clear_pointer(&self->variable_defs, g_hash_table_unref);
  g_clear_pointer(&self->variable_uses, g_hash_table_unref);
  g_clear_pointer(&self->package_defs, g_hash_table_unref);
  g_clear_pointer(&self->package_uses, g_hash_table_unref);
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

void project_file_set_provider(Project *self, ProjectFile *file,
    TextProvider *provider, GtkTextBuffer *buffer) {
  g_return_if_fail(GLIDE_IS_PROJECT(self));
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
  project_file_changed(self, file);
}

TextProvider *project_file_get_provider(ProjectFile *file) {
  g_return_val_if_fail(file != NULL, NULL);
  return file->provider;
}

static GHashTable *project_index_table(Project *self, NodeInfoKind kind) {
  switch(kind) {
    case NODE_INFO_FUNCTION_DEF: return self->function_defs;
    case NODE_INFO_FUNCTION_USE: return self->function_uses;
    case NODE_INFO_VAR_DEF: return self->variable_defs;
    case NODE_INFO_VAR_USE: return self->variable_uses;
    case NODE_INFO_PACKAGE_DEF: return self->package_defs;
    case NODE_INFO_PACKAGE_USE: return self->package_uses;
    default: return NULL;
  }
}

static void project_index_clear(Project *self) {
  GHashTable *tables[] = { self->function_defs, self->function_uses,
    self->variable_defs, self->variable_uses, self->package_defs,
    self->package_uses };
  for (guint t = 0; t < G_N_ELEMENTS(tables); t++)
    if (tables[t])
      g_hash_table_remove_all(tables[t]);
}

void project_index_add(Project *self, NodeInfo *ni, const LispAstNode *node) {
  g_return_if_fail(GLIDE_IS_PROJECT(self));
  if (!ni || !node) return;
  const gchar *name = node_info_get_name(ni, node);
  if (!name) return;
  GHashTable *table = project_index_table(self, ni->kind);
  if (!table) return;
  GPtrArray *arr = g_hash_table_lookup(table, name);
  if (!arr) {
    arr = g_ptr_array_new_with_free_func((GDestroyNotify)node_info_unref);
    g_hash_table_insert(table, g_strdup(name), arr);
  }
  g_ptr_array_add(arr, node_info_ref(ni));
}

GHashTable *project_get_index(Project *self, NodeInfoKind kind) {
  g_return_val_if_fail(GLIDE_IS_PROJECT(self), NULL);
  return project_index_table(self, kind);
}

static void project_index_node(Project *self, const LispAstNode *node) {
  if (!node || !node->node_info) return;
  project_index_add(self, node->node_info, node);
}

static void project_index_walk(Project *self, const LispAstNode *node) {
  if (!node) return;
  project_index_node(self, node);
  if (node->children)
    for (guint i = 0; i < node->children->len; i++)
      project_index_walk(self, g_array_index(node->children, LispAstNode*, i));
}

void project_file_changed(Project *self, ProjectFile *file) {
  g_return_if_fail(GLIDE_IS_PROJECT(self));
  g_return_if_fail(file != NULL);
  if (!file->lexer || !file->parser)
    return;
  lisp_lexer_lex(file->lexer);
  GArray *tokens = lisp_lexer_get_tokens(file->lexer);
  lisp_parser_parse(file->parser, tokens);
  const LispAstNode *ast = lisp_parser_get_ast(file->parser);
  if (ast)
    analyse_ast((LispAstNode*)ast);
  project_index_clear(self);
  for (guint i = 0; i < self->files->len; i++) {
    ProjectFile *f = g_ptr_array_index(self->files, i);
    const LispAstNode *a = lisp_parser_get_ast(f->parser);
    if (a)
      project_index_walk(self, a);
  }
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
  project_file_set_provider(self, file, provider, NULL);
  g_object_unref(provider);
  project_file_set_state(file, PROJECT_FILE_LIVE);

  g_signal_emit(self, project_signals[FILE_LOADED], 0, file);

  g_free(content);
  return TRUE;
}
