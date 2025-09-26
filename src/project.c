#include "project_priv.h"
#include "analyse.h"
#include "lisp_lexer.h"
#include "lisp_parser.h"
#include "util.h"
#include "project_repl.h"
#include <glib-object.h>

static Project *project_init(void) {
  Project *self = g_new0(Project, 1);
  self->refcnt = 1;
  self->files = g_ptr_array_new_with_free_func((GDestroyNotify)project_file_free);
  self->index = project_index_new();
  self->asdf = asdf_new();
  self->repl = NULL;
  self->path = NULL;
  self->changed_cb = NULL;
  self->changed_data = NULL;
  self->file_loaded_cb = NULL;
  self->file_loaded_data = NULL;
  self->file_removed_cb = NULL;
  self->file_removed_data = NULL;
  self->file_changed_cb = NULL;
  self->file_changed_data = NULL;
  return self;
}

static void project_free(Project *self) {
  g_clear_pointer(&self->index, project_index_free);
  if (self->files)
    g_ptr_array_free(self->files, TRUE);
  g_clear_object(&self->asdf);
  g_clear_pointer(&self->repl, repl_session_unref);
  g_free(self->path);
  g_free(self);
}

Project *project_new(ReplSession *repl) {
  g_return_val_if_fail(glide_is_ui_thread(), NULL);
  LOG(1, "project_new");
  Project *self = project_init();
  self->repl = repl ? repl_session_ref(repl) : NULL;
  project_clear(self);
  GString *content = g_string_new("");
  project_add_file(self, content, "unnamed.lisp", PROJECT_FILE_LIVE);
  return self;
}

ProjectFile *project_add_file(Project *self, GString *content,
    const gchar *path, ProjectFileState state) {
  g_return_val_if_fail(self != NULL, NULL);
  g_return_val_if_fail(glide_is_ui_thread(), NULL);

  LOG(1, "project_add_file path=%s state=%d", path ? path : "(null)", state);

  if (!content)
    content = g_string_new("");
  ProjectFile *file = project_file_new(self, content, path, state);

  g_ptr_array_add(self->files, file);

  project_file_loaded(self, file);
  project_changed(self);

  return file;
}

ProjectFile *project_add_loaded_file(Project *self, const gchar *path) {
  g_return_val_if_fail(self != NULL, NULL);
  g_return_val_if_fail(path != NULL, NULL);
  g_return_val_if_fail(glide_is_ui_thread(), NULL);

  LOG(1, "project_add_loaded_file path=%s", path);

  ProjectFile *file = project_file_load(self, path);
  if (!file)
    return NULL;

  g_ptr_array_add(self->files, file);

  project_changed(self);
  project_file_loaded(self, file);

  return file;
}

void project_remove_file(Project *self, ProjectFile *file) {
  g_return_if_fail(self != NULL);
  g_return_if_fail(file != NULL);
  g_return_if_fail(glide_is_ui_thread());
  for (guint i = 0; i < self->files->len; i++) {
    if (g_ptr_array_index(self->files, i) == file) {
      project_file_removed(self, file);
      g_ptr_array_remove_index(self->files, i);
      break;
    }
  }
  project_index_clear(self->index);
  for (guint i = 0; i < self->files->len; i++) {
    ProjectFile *f = g_ptr_array_index(self->files, i);
    const Node *a = project_file_get_ast(f);
    if (a)
      project_index_walk(self->index, a);
  }

  project_changed(self);
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

GHashTable *project_get_index(Project *self, StringDesignatorType sd_type) {
  g_return_val_if_fail(self != NULL, NULL);
  return project_index_get(self->index, sd_type);
}

void project_add_package(Project *self, Package *package) {
  g_return_if_fail(self != NULL);
  g_return_if_fail(package != NULL);
  g_return_if_fail(glide_is_ui_thread());
  project_index_add_package(self->index, package);
  project_changed(self);
}

void project_add_function(Project *self, Function *function) {
  g_return_if_fail(self != NULL);
  g_return_if_fail(function != NULL);
  g_return_if_fail(glide_is_ui_thread());
  project_index_add_function(self->index, function);
  project_changed(self);
}

Function *project_get_function(Project *self, const gchar *name) {
  g_return_val_if_fail(self != NULL, NULL);
  g_return_val_if_fail(name != NULL, NULL);
  return project_index_get_function(self->index, name);
}

gchar **project_get_function_names(Project *self, const gchar *package,
    guint *length) {
  g_return_val_if_fail(self != NULL, NULL);
  return project_index_get_function_names(self->index, package, length);
}

void project_add_variable(Project *self, const gchar *package,
    const gchar *name, const gchar *doc) {
  g_return_if_fail(self != NULL);
  g_return_if_fail(package != NULL);
  g_return_if_fail(name != NULL);
  g_return_if_fail(glide_is_ui_thread());
  project_index_add_variable(self->index, package, name, doc);
  project_changed(self);
}

const gchar *project_get_variable(Project *self, const gchar *name) {
  g_return_val_if_fail(self != NULL, NULL);
  g_return_val_if_fail(name != NULL, NULL);
  return project_index_get_variable(self->index, name);
}

gchar **project_get_variable_names(Project *self, const gchar *package,
    guint *length) {
  g_return_val_if_fail(self != NULL, NULL);
  return project_index_get_variable_names(self->index, package, length);
}

Package *project_get_package(Project *self, const gchar *name) {
  g_return_val_if_fail(self != NULL, NULL);
  g_return_val_if_fail(name != NULL, NULL);
  return project_index_get_package(self->index, name);
}

gchar **project_get_package_names(Project *self, guint *length) {
  g_return_val_if_fail(self != NULL, NULL);
  return project_index_get_package_names(self->index, length);
}

void project_set_asdf(Project *self, Asdf *asdf) {
  g_return_if_fail(self != NULL);
  g_return_if_fail(glide_is_ui_thread());
  LOG(1, "project_set_asdf %p", asdf);
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
  g_return_if_fail(glide_is_ui_thread());
  g_free(self->path);
  self->path = path ? g_strdup(path) : NULL;
}

void project_clear(Project *self) {
  g_return_if_fail(self != NULL);
  g_return_if_fail(glide_is_ui_thread());
  LOG(1, "project_clear");
  project_index_clear(self->index);
  if (self->files) {
    for (guint i = 0; i < self->files->len; i++) {
      ProjectFile *file = g_ptr_array_index(self->files, i);
      project_file_removed(self, file);
    }
    g_ptr_array_set_size(self->files, 0);
  }
  Asdf *asdf = asdf_new();
  project_set_asdf(self, asdf);
  g_object_unref(asdf);
  if (self->repl) {
    project_request_package(self, "COMMON-LISP");
    project_request_package(self, "COMMON-LISP-USER");
  }
  project_changed(self);
}

void project_file_changed(Project *self, ProjectFile *file) {
  g_return_if_fail(self != NULL);
  g_return_if_fail(file != NULL);
  g_return_if_fail(glide_is_ui_thread());
  LOG(1, "project_file_changed path=%s", project_file_get_path(file));
  if (!project_file_get_lexer(file) || !project_file_get_parser(file))
    return;
  project_file_clear_errors(file);
  project_index_remove_file(self->index, file);
  LispLexer *lexer = project_file_get_lexer(file);
  LispParser *parser = project_file_get_parser(file);
  const GString *content = project_file_get_content(file);
  if (!content)
    return;
  GArray *tokens = lisp_lexer_lex(lexer, content);
  project_file_set_tokens(file, tokens);
  Node *ast = lisp_parser_parse(parser, tokens, file);
  project_file_set_ast(file, ast);
  if (ast)
    analyse_ast(self, ast);
  if (ast)
    project_index_walk(self->index, ast);
  if (self->file_changed_cb)
    self->file_changed_cb(self, file, self->file_changed_data);
  project_changed(self);
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
  g_return_if_fail(glide_is_ui_thread());
  self->file_loaded_cb = cb;
  self->file_loaded_data = user_data;
}

void project_set_file_changed_cb(Project *self, ProjectFileChangedCb cb, gpointer user_data) {
  g_return_if_fail(self != NULL);
  g_return_if_fail(glide_is_ui_thread());
  self->file_changed_cb = cb;
  self->file_changed_data = user_data;
}

void project_file_loaded(Project *self, ProjectFile *file) {
  g_return_if_fail(self != NULL);
  g_return_if_fail(file != NULL);
  g_return_if_fail(glide_is_ui_thread());
  if (self->file_loaded_cb)
    self->file_loaded_cb(self, file, self->file_loaded_data);
  project_changed(self);
}

void project_set_file_removed_cb(Project *self, ProjectFileRemovedCb cb, gpointer user_data) {
  g_return_if_fail(self != NULL);
  g_return_if_fail(glide_is_ui_thread());
  self->file_removed_cb = cb;
  self->file_removed_data = user_data;
}

void project_file_removed(Project *self, ProjectFile *file) {
  g_return_if_fail(self != NULL);
  g_return_if_fail(file != NULL);
  g_return_if_fail(glide_is_ui_thread());
  if (self->file_removed_cb)
    self->file_removed_cb(self, file, self->file_removed_data);
}

void project_set_changed_cb(Project *self, ProjectChangedCb cb, gpointer user_data) {
  g_return_if_fail(self != NULL);
  g_return_if_fail(glide_is_ui_thread());
  self->changed_cb = cb;
  self->changed_data = user_data;
}

void project_changed(Project *self) {
  g_return_if_fail(self != NULL);
  g_return_if_fail(glide_is_ui_thread());
  LOG(1, "project_changed");
  if (self->changed_cb)
    self->changed_cb(self, self->changed_data);
}

