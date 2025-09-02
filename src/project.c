#include "project.h"
#include "string_text_provider.h"
#include "analyse.h"
#include "analyse_defpackage.h"
#include "lisp_lexer.h"
#include "lisp_parser.h"
#include "repl_session.h"
#include "interaction.h"
#include "asdf.h"
#include "util.h"
#include <glib-object.h>

struct _Project {
  GPtrArray *files; /* ProjectFile* */
  GHashTable *function_defs; /* name -> GPtrArray* Node* */
  GHashTable *function_uses;
  GHashTable *variable_defs;
  GHashTable *variable_uses;
  GHashTable *package_defs;
  GHashTable *package_uses;
  GHashTable *packages; /* name -> Package* */
  GHashTable *functions; /* name -> Function* */
  ProjectFileLoadedCb file_loaded_cb;
  gpointer file_loaded_data;
  ProjectFileRemovedCb file_removed_cb;
  gpointer file_removed_data;
  ProjectPackageAddedCb package_added_cb;
  gpointer package_added_data;
  Asdf *asdf; /* owned, nullable */
  gchar *path;
  gint refcnt;
};

static void project_index_clear(Project *self);
static void project_index_node(Project *self, const Node *node);
static void project_index_walk(Project *self, const Node *node);
static GHashTable *project_index_table(Project *self, StringDesignatorType sd_type);
static void project_on_package_definition(Interaction *interaction, gpointer user_data);
static void project_request_package(Project *self, ReplSession *repl, const gchar *name);

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
  self->packages = g_hash_table_new_full(g_str_hash, g_str_equal, g_free, (GDestroyNotify)package_unref);
  self->functions = g_hash_table_new_full(g_str_hash, g_str_equal, g_free, (GDestroyNotify)function_unref);
  self->asdf = asdf_new();
  self->path = NULL;
  self->package_added_cb = NULL;
  self->package_added_data = NULL;
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
  g_clear_pointer(&self->packages, g_hash_table_unref);
  g_clear_pointer(&self->functions, g_hash_table_unref);
  if (self->files)
    g_ptr_array_free(self->files, TRUE);
  g_clear_object(&self->asdf);
  g_free(self->path);
  g_free(self);
}

static void project_on_package_definition(Interaction *interaction, gpointer user_data) {
  Project *project = user_data;
  gchar *res = NULL;
  g_mutex_lock(&interaction->lock);
  if (interaction->result)
    res = g_strdup(interaction->result);
  g_mutex_unlock(&interaction->lock);
  if (res) {
    TextProvider *provider = string_text_provider_new(res);
    LispLexer *lexer = lisp_lexer_new(provider);
    lisp_lexer_lex(lexer);
    GArray *tokens = lisp_lexer_get_tokens(lexer);
    LispParser *parser = lisp_parser_new();
    lisp_parser_parse(parser, tokens, NULL);
    const Node *ast = lisp_parser_get_ast(parser);
    if (ast && ast->children && ast->children->len > 0) {
      Node *expr = g_array_index(ast->children, Node*, 0);
      Node *name_node = (expr->children && expr->children->len > 1) ?
        g_array_index(expr->children, Node*, 1) : NULL;
      const gchar *pkg_name = node_get_name(name_node);
      if (pkg_name) {
        analyse_defpackage(project, expr, NULL);
        g_debug("project_on_package_definition built package %s", pkg_name);
      } else {
        g_debug_160("project_on_package_definition failed, missing name in ", res);
      }
    } else {
      g_debug_160("project_on_package_definition failed to parse ", res);
    }
    lisp_parser_free(parser);
    lisp_lexer_free(lexer);
    text_provider_unref(provider);
  } else {
    g_debug("project_on_package_definition no result");
  }
  project_unref(project);
  interaction_clear(interaction);
  g_free(interaction);
  g_free(res);
}

static void project_request_package(Project *self, ReplSession *repl, const gchar *name) {
  g_return_if_fail(self);
  g_return_if_fail(repl);
  g_return_if_fail(name);
  gchar *expr = g_strdup_printf("(glide:package-definition \"%s\")", name);
  Interaction *interaction = g_new0(Interaction, 1);
  interaction_init(interaction, expr);
  g_mutex_lock(&interaction->lock);
  interaction->type = INTERACTION_INTERNAL;
  interaction->done_cb = project_on_package_definition;
  interaction->done_cb_data = project_ref(self);
  g_mutex_unlock(&interaction->lock);
  repl_session_eval(repl, interaction);
  g_free(expr);
}

Project *project_new(ReplSession *repl) {
  g_debug("project_new");
  Project *self = project_init();
  TextProvider *provider = string_text_provider_new("");
  project_add_file(self, provider, NULL, "unnamed.lisp", PROJECT_FILE_LIVE);
  text_provider_unref(provider);
  if (repl) {
    project_request_package(self, repl, "COMMON-LISP");
    project_request_package(self, repl, "COMMON-LISP-USER");
  }
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
  if (self->packages)
    g_hash_table_remove_all(self->packages);
  if (self->functions)
    g_hash_table_remove_all(self->functions);
}

static void project_index_add_to(GHashTable *table, const gchar *name, Node *node) {
  if (!table || !name || !node) return;
  GPtrArray *arr = g_hash_table_lookup(table, name);
  if (!arr) {
    arr = g_ptr_array_new_with_free_func((GDestroyNotify)node_unref);
    g_hash_table_insert(table, g_strdup(name), arr);
  }
  g_ptr_array_add(arr, node_ref(node));
}

void project_index_add(Project *self, Node *node) {
  if (!self || !node || !node->sd_type)
    return;
  GHashTable *table = project_index_table(self, node->sd_type);
  if (!table) return;
  const gchar *name = node_get_name(node);
  project_index_add_to(table, name, node);
}

GHashTable *project_get_index(Project *self, StringDesignatorType sd_type) {
  g_return_val_if_fail(self != NULL, NULL);
  return project_index_table(self, sd_type);
}

void project_add_package(Project *self, Package *package) {
  g_return_if_fail(self != NULL);
  g_return_if_fail(package != NULL);
  const gchar *name = package_get_name(package);
  if (!name) return;
  g_debug("project_add_package %s", name);
  g_hash_table_replace(self->packages, g_strdup(name), package_ref(package));
  if (self->package_added_cb)
    self->package_added_cb(self, package, self->package_added_data);
}

void project_add_function(Project *self, Function *function) {
  g_return_if_fail(self != NULL);
  g_return_if_fail(function != NULL);
  const gchar *name = function_get_name(function);
  if (!name) return;
  g_hash_table_replace(self->functions, g_strdup(name), function_ref(function));
}

Function *project_get_function(Project *self, const gchar *name) {
  g_return_val_if_fail(self != NULL, NULL);
  g_return_val_if_fail(name != NULL, NULL);
  return g_hash_table_lookup(self->functions, name);
}

Package *project_get_package(Project *self, const gchar *name) {
  g_return_val_if_fail(self != NULL, NULL);
  g_return_val_if_fail(name != NULL, NULL);
  return g_hash_table_lookup(self->packages, name);
}

gchar **project_get_package_names(Project *self, guint *length) {
  g_return_val_if_fail(self != NULL, NULL);
  return (gchar **) g_hash_table_get_keys_as_array(self->packages, length);
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
  g_hash_table_remove_all(self->packages);
  g_hash_table_remove_all(self->functions);
  Asdf *asdf = asdf_new();
  project_set_asdf(self, asdf);
  g_object_unref(asdf);
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

static void project_index_remove_from_table(GHashTable *table, ProjectFile *file) {
  if (!table) return;
  GHashTableIter iter;
  g_hash_table_iter_init(&iter, table);
  gpointer key, value;
  while (g_hash_table_iter_next(&iter, &key, &value)) {
    GPtrArray *arr = value;
    for (guint i = 0; i < arr->len; ) {
      Node *node = g_ptr_array_index(arr, i);
      if (node->file == file)
        g_ptr_array_remove_index(arr, i);
      else
        i++;
    }
    if (arr->len == 0)
      g_hash_table_iter_remove(&iter);
  }
}

static void project_index_remove_file(Project *self, ProjectFile *file) {
  GHashTable *tables[] = { self->function_defs, self->function_uses,
    self->variable_defs, self->variable_uses, self->package_defs,
    self->package_uses };
  for (guint t = 0; t < G_N_ELEMENTS(tables); t++)
    project_index_remove_from_table(tables[t], file);
}

static void project_functions_remove_file(Project *self, ProjectFile *file) {
  if (!self->functions) return;
  GHashTableIter iter;
  g_hash_table_iter_init(&iter, self->functions);
  gpointer key, value;
  while (g_hash_table_iter_next(&iter, &key, &value)) {
    Function *fn = value;
    const Node *sym = function_get_symbol(fn);
    if (sym && sym->file == file)
      g_hash_table_iter_remove(&iter);
  }
}

static void project_packages_prune(Project *self) {
  if (!self->packages) return;
  GHashTableIter iter;
  g_hash_table_iter_init(&iter, self->packages);
  gpointer key, value;
  while (g_hash_table_iter_next(&iter, &key, &value))
    if (!g_hash_table_lookup(self->package_defs, key))
      g_hash_table_iter_remove(&iter);
}

void project_file_changed(Project *self, ProjectFile *file) {
  g_return_if_fail(self != NULL);
  g_return_if_fail(file != NULL);
  g_debug("project_file_changed path=%s", project_file_get_path(file));
  if (!project_file_get_lexer(file) || !project_file_get_parser(file))
    return;
  project_index_remove_file(self, file);
  project_functions_remove_file(self, file);
  project_packages_prune(self);
  LispLexer *lexer = project_file_get_lexer(file);
  LispParser *parser = project_file_get_parser(file);
  lisp_lexer_lex(lexer);
  GArray *tokens = lisp_lexer_get_tokens(lexer);
  lisp_parser_parse(parser, tokens, file);
  const Node *ast = lisp_parser_get_ast(parser);
  if (ast)
    analyse_ast(self, (Node*)ast);
  if (ast)
    project_index_walk(self, ast);
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

void project_set_package_added_cb(Project *self, ProjectPackageAddedCb cb, gpointer user_data) {
  g_return_if_fail(self != NULL);
  self->package_added_cb = cb;
  self->package_added_data = user_data;
}

