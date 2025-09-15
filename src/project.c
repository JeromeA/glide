#include "project_priv.h"
#include "string_text_provider.h"
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
  TextProvider *provider = string_text_provider_new("");
  project_add_file(self, provider, NULL, "unnamed.lisp", PROJECT_FILE_LIVE);
  text_provider_unref(provider);
  if (self->repl) {
    project_request_package(self, "COMMON-LISP");
    project_request_package(self, "COMMON-LISP-USER");
  }
  return self;
}

ProjectFile *project_add_file(Project *self, TextProvider *provider,
    GtkTextBuffer *buffer, const gchar *path, ProjectFileState state) {
  g_return_val_if_fail(self != NULL, NULL);
  g_return_val_if_fail(provider, NULL);
  g_return_val_if_fail(glide_is_ui_thread(), NULL);

  LOG(1, "project_add_file path=%s state=%d", path ? path : "(null)", state);

  ProjectFile *file = project_file_new(self, provider, buffer, path, state);

  g_ptr_array_add(self->files, file);

  project_changed(self);

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
    const Node *a = lisp_parser_get_ast(project_file_get_parser(f));
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

gchar *project_function_tooltip(Function *function) {
  g_return_val_if_fail(function != NULL, NULL);
  GString *tt = g_string_new(NULL);
  const Node *lambda = function_get_lambda_list(function);
  const gchar *pkg = function_get_package(function);
  const gchar *name = function_get_name(function);
  if (lambda && name) {
    gchar *ls = node_to_string(lambda);
    gsize len = strlen(ls);
    gchar *pkg_esc = pkg ? g_markup_escape_text(pkg, -1) : NULL;
    gchar *name_esc = g_markup_escape_text(name, -1);
    if (pkg_esc)
      g_string_append_printf(tt,
          "In <span foreground=\"darkgreen\">%s</span>:\n", pkg_esc);
    g_string_append_printf(tt, "(<span foreground=\"brown\">%s</span>",
        name_esc);
    if (len > 2 && ls[0] == '(' && ls[len - 1] == ')') {
      gchar *args = g_strndup(ls + 1, len - 2);
      g_string_append_c(tt, ' ');
      gchar **tokens = g_strsplit_set(args, " \t\n", 0);
      gboolean first = TRUE;
      for (guint i = 0; tokens[i]; i++) {
        if (!tokens[i][0])
          continue;
        if (!first)
          g_string_append_c(tt, ' ');
        gchar *tok_esc = g_markup_escape_text(tokens[i], -1);
        if (tokens[i][0] == '&')
          g_string_append_printf(tt,
              "<span foreground=\"darkgreen\">%s</span>", tok_esc);
        else
          g_string_append(tt, tok_esc);
        g_free(tok_esc);
        first = FALSE;
      }
      g_strfreev(tokens);
      g_free(args);
    }
    g_string_append_c(tt, ')');
    g_free(pkg_esc);
    g_free(name_esc);
    g_free(ls);
  }
  const Node *sym = function_get_symbol(function);
  if (sym && sym->file) {
    const gchar *rel = project_file_get_relative_path(sym->file);
    if (rel) {
      gchar *rel_esc = g_markup_escape_text(rel, -1);
      if (tt->len)
        g_string_append_c(tt, '\n');
      g_string_append_printf(tt, "File: %s", rel_esc);
      g_free(rel_esc);
    }
  }
  const gchar *doc = function_get_doc_string(function);
  if (doc && *doc) {
    if (tt->len)
      g_string_append_c(tt, '\n');
    gchar *doc_esc = g_markup_escape_text(doc, -1);
    g_string_append(tt, doc_esc);
    g_free(doc_esc);
  }
  if (!tt->len) {
    g_string_free(tt, TRUE);
    return NULL;
  }
  return g_string_free(tt, FALSE);
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
  project_changed(self);
}

void project_file_changed(Project *self, ProjectFile *file) {
  g_return_if_fail(self != NULL);
  g_return_if_fail(file != NULL);
  g_return_if_fail(glide_is_ui_thread());
  LOG(1, "project_file_changed path=%s", project_file_get_path(file));
  if (!project_file_get_lexer(file) || !project_file_get_parser(file))
    return;
  project_index_remove_file(self->index, file);
  LispLexer *lexer = project_file_get_lexer(file);
  LispParser *parser = project_file_get_parser(file);
  lisp_lexer_lex(lexer);
  GArray *tokens = lisp_lexer_get_tokens(lexer);
  lisp_parser_parse(parser, tokens, file);
  const Node *ast = lisp_parser_get_ast(parser);
  if (ast)
    analyse_ast(self, (Node*)ast);
  if (ast)
    project_index_walk(self->index, ast);
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

