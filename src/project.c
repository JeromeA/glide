#include "project.h"
#include "string_text_provider.h"
#include "analyser.h"
#include <glib-object.h>

struct _Project {
  GPtrArray *files; /* ProjectFile* */
  GHashTable *function_defs; /* name -> GPtrArray* Node* */
  GHashTable *function_uses;
  GHashTable *variable_defs;
  GHashTable *variable_uses;
  GHashTable *package_defs;
  GHashTable *package_uses;
  ProjectFileLoadedCb file_loaded_cb;
  gpointer file_loaded_data;
  ProjectFileRemovedCb file_removed_cb;
  gpointer file_removed_data;
  Asdf *asdf; /* owned, nullable */
  gchar *path;
  gint refcnt;
};

static void project_index_clear(Project *self);
static void project_index_node(Project *self, const Node *node);
static void project_index_walk(Project *self, const Node *node);
static GHashTable *project_index_table(Project *self, StringDesignatorType sd_type);

static Project *project_init(void) {
  Project *self = g_new0(Project, 1);
  self->refcnt = 1;
  self->files = g_ptr_array_new_with_free_func((GDestroyNotify)project_file_free);
  self->function_defs = g_hash_table_new_full(g_str_hash, g_str_equal, g_free, (GDestroyNotify)g_ptr_array_unref);
  self->function_uses = g_hash_table_new_full(g_str_hash, g_str_equal, g_free, (GDestroyNotify)g_ptr_array_unref);
  self->variable_defs = g_hash_table_new_full(g_str_hash, g_str_equal, g_free, (GDestroyNotify)g_ptr_array_unref);
  self->variable_uses = g_hash_table_new_full(g_str_hash, g_str_equal, g_free, (GDestroyNotify)g_ptr_array_unref);
  self->package_defs = g_hash_table_new_full(g_str_hash, g_str_equal, g_free, (GDestroyNotify)g_ptr_array_unref);
  self->package_uses = g_hash_table_new_full(g_str_hash, g_str_equal, g_free, (GDestroyNotify)g_ptr_array_unref);
  self->asdf = NULL;
  self->path = NULL;
  return self;
}

static void project_free(Project *self) {
  project_index_clear(self);
  g_clear_pointer(&self->function_defs, g_hash_table_unref);
  g_clear_pointer(&self->function_uses, g_hash_table_unref);
  g_clear_pointer(&self->variable_defs, g_hash_table_unref);
  g_clear_pointer(&self->variable_uses, g_hash_table_unref);
  g_clear_pointer(&self->package_defs, g_hash_table_unref);
  g_clear_pointer(&self->package_uses, g_hash_table_unref);
  if (self->files)
    g_ptr_array_free(self->files, TRUE);
  g_clear_object(&self->asdf);
  g_free(self->path);
  g_free(self);
}

Project *project_new(void) {
  g_debug("project_new");
  Project *self = project_init();
  TextProvider *provider = string_text_provider_new("");
  project_add_file(self, provider, NULL, "unnamed.lisp", PROJECT_FILE_LIVE);
  text_provider_unref(provider);
  return self;
}

ProjectFile *project_add_file(Project *self, TextProvider *provider,
    GtkTextBuffer *buffer, const gchar *path, ProjectFileState state) {
  g_return_val_if_fail(self != NULL, NULL);
  g_return_val_if_fail(provider, NULL);

  g_debug("project_add_file path=%s state=%d", path ? path : "(null)", state);

  ProjectFile *file = project_file_new(self, provider, buffer, path, state);

  g_ptr_array_add(self->files, file);

  return file;
}

void project_remove_file(Project *self, ProjectFile *file) {
  g_return_if_fail(self != NULL);
  g_return_if_fail(file != NULL);
  for (guint i = 0; i < self->files->len; i++) {
    if (g_ptr_array_index(self->files, i) == file) {
      project_file_removed(self, file);
      g_ptr_array_remove_index(self->files, i);
      break;
    }
  }
  project_index_clear(self);
  for (guint i = 0; i < self->files->len; i++) {
    ProjectFile *f = g_ptr_array_index(self->files, i);
    const Node *a = lisp_parser_get_ast(project_file_get_parser(f));
    if (a)
      project_index_walk(self, a);
  }
}

guint project_get_file_count(Project *self) {
  g_return_val_if_fail(self != NULL, 0);
  return self->files->len;
}

ProjectFile *project_get_file(Project *self, guint index) {
  g_return_val_if_fail(self != NULL, NULL);
  if (index >= self->files->len)
    return NULL;
  return g_ptr_array_index(self->files, index);
}

static GHashTable *project_index_table(Project *self, StringDesignatorType sd_type) {
  switch(sd_type) {
    case SDT_FUNCTION_DEF: return self->function_defs;
    case SDT_FUNCTION_USE: return self->function_uses;
    case SDT_VAR_DEF: return self->variable_defs;
    case SDT_VAR_USE: return self->variable_uses;
    case SDT_PACKAGE_DEF: return self->package_defs;
    case SDT_PACKAGE_USE: return self->package_uses;
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

void project_index_add(Project *self, Node *node) {
  g_return_if_fail(self != NULL);
  if (!node) return;
  const gchar *name = node_get_name(node);
  if (!name) return;
  GHashTable *table = project_index_table(self, node->sd_type);
  if (!table) return;
  GPtrArray *arr = g_hash_table_lookup(table, name);
  if (!arr) {
    arr = g_ptr_array_new_with_free_func((GDestroyNotify)node_unref);
    g_hash_table_insert(table, g_strdup(name), arr);
  }
  g_ptr_array_add(arr, node_ref(node));
}

GHashTable *project_get_index(Project *self, StringDesignatorType sd_type) {
  g_return_val_if_fail(self != NULL, NULL);
  return project_index_table(self, sd_type);
}

void project_set_asdf(Project *self, Asdf *asdf) {
  g_return_if_fail(self != NULL);
  g_debug("project_set_asdf %p", asdf);
  if (self->asdf)
    g_object_unref(self->asdf);
  self->asdf = asdf ? g_object_ref(asdf) : NULL;
}

Asdf *project_get_asdf(Project *self) {
  g_return_val_if_fail(self != NULL, NULL);
  return self->asdf;
}

const gchar *project_get_path(Project *self) {
  g_return_val_if_fail(self != NULL, NULL);
  return self->path;
}

void project_set_path(Project *self, const gchar *path) {
  g_return_if_fail(self != NULL);
  g_free(self->path);
  self->path = path ? g_strdup(path) : NULL;
}

void project_clear(Project *self) {
  g_return_if_fail(self != NULL);
  g_debug("project_clear");
  project_index_clear(self);
  if (self->files) {
    for (guint i = 0; i < self->files->len; i++) {
      ProjectFile *file = g_ptr_array_index(self->files, i);
      project_file_removed(self, file);
    }
    g_ptr_array_set_size(self->files, 0);
  }
  project_set_asdf(self, NULL);
}

static void project_index_node(Project *self, const Node *node) {
  if (!node || !node->sd_type) return;
  project_index_add(self, (Node*)node);
}

static void project_index_walk(Project *self, const Node *node) {
  if (!node) return;
  project_index_node(self, node);
  if (node->children)
    for (guint i = 0; i < node->children->len; i++)
      project_index_walk(self, g_array_index(node->children, Node*, i));
}

void project_file_changed(Project *self, ProjectFile *file) {
  g_return_if_fail(self != NULL);
  g_return_if_fail(file != NULL);
  g_debug("project_file_changed path=%s", project_file_get_path(file));
  if (!project_file_get_lexer(file) || !project_file_get_parser(file))
    return;
  project_index_clear(self);
  LispLexer *lexer = project_file_get_lexer(file);
  LispParser *parser = project_file_get_parser(file);
  lisp_lexer_lex(lexer);
  GArray *tokens = lisp_lexer_get_tokens(lexer);
  lisp_parser_parse(parser, tokens);
  const Node *ast = lisp_parser_get_ast(parser);
  if (ast)
    analyse_ast((Node*)ast);
  for (guint i = 0; i < self->files->len; i++) {
    ProjectFile *f = g_ptr_array_index(self->files, i);
    const Node *a = lisp_parser_get_ast(project_file_get_parser(f));
    if (a)
      project_index_walk(self, a);
  }
}

Project *project_ref(Project *self) {
  g_return_val_if_fail(self != NULL, NULL);
  g_atomic_int_inc(&self->refcnt);
  return self;
}

void project_unref(Project *self) {
  if (!self)
    return;
  if (g_atomic_int_dec_and_test(&self->refcnt))
    project_free(self);
}

void project_set_file_loaded_cb(Project *self, ProjectFileLoadedCb cb, gpointer user_data) {
  g_return_if_fail(self != NULL);
  self->file_loaded_cb = cb;
  self->file_loaded_data = user_data;
}

void project_file_loaded(Project *self, ProjectFile *file) {
  g_return_if_fail(self != NULL);
  g_return_if_fail(file != NULL);
  if (self->file_loaded_cb)
    self->file_loaded_cb(self, file, self->file_loaded_data);
}

void project_set_file_removed_cb(Project *self, ProjectFileRemovedCb cb, gpointer user_data) {
  g_return_if_fail(self != NULL);
  self->file_removed_cb = cb;
  self->file_removed_data = user_data;
}

void project_file_removed(Project *self, ProjectFile *file) {
  g_return_if_fail(self != NULL);
  g_return_if_fail(file != NULL);
  if (self->file_removed_cb)
    self->file_removed_cb(self, file, self->file_removed_data);
}
