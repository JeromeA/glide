#include "function.h"
#include "project_file.h"
#include <string.h>

struct Function {
  gint ref;
  Node *symbol;
  Node *lambda_list;
  gchar *doc_string;
  Node *type;
  gchar *name;
  gchar *package;
  FunctionKind kind;
  ProjectFile *file;
};

Function *
function_new(Node *symbol, Node *lambda_list, const gchar *doc_string,
    Node *type, FunctionKind kind, const gchar *name, const gchar *package,
    ProjectFile *file)
{
  Function *function = g_new0(Function, 1);
  g_atomic_int_set(&function->ref, 1);
  Node *symbol_name = symbol ? node_get_symbol_name_node(symbol) : NULL;
  function->symbol = symbol ? node_ref(symbol) : NULL;
  function->lambda_list = lambda_list ? node_ref(lambda_list) : NULL;
  function->doc_string = doc_string ? g_strdup(doc_string) : NULL;
  function->type = type ? node_ref(type) : NULL;
  function->name = name ? g_strdup(name) :
    (symbol ? g_strdup(node_get_name(symbol)) : NULL);
  function->package = package ? g_strdup(package) :
    (symbol_name ? g_strdup(symbol_name->package_context) : NULL);
  function->kind = kind;
  function->file = file;
  return function;
}

Function *
function_ref(Function *function)
{
  if (!function)
    return NULL;
  g_atomic_int_inc(&function->ref);
  return function;
}

static void
function_finalize(Function *function)
{
  node_unref(function->symbol);
  if (function->lambda_list)
    node_unref(function->lambda_list);
  if (function->type)
    node_unref(function->type);
  g_clear_pointer(&function->doc_string, g_free);
  g_clear_pointer(&function->name, g_free);
  g_clear_pointer(&function->package, g_free);
  if (function->file)
    project_file_free(function->file);
}

void
function_unref(Function *function)
{
  if (!function)
    return;
  if (g_atomic_int_dec_and_test(&function->ref)) {
    function_finalize(function);
    g_free(function);
  }
}

const Node *
function_get_symbol(const Function *function)
{
  return function ? function->symbol : NULL;
}

const Node *
function_get_lambda_list(const Function *function)
{
  return function ? function->lambda_list : NULL;
}

const gchar *
function_get_doc_string(const Function *function)
{
  return function ? function->doc_string : NULL;
}

const Node *
function_get_type(const Function *function)
{
  return function ? function->type : NULL;
}

FunctionKind
function_get_kind(const Function *function)
{
  return function ? function->kind : FUNCTION_KIND_FUNCTION;
}

static const gchar *
function_get_kind_name(FunctionKind kind)
{
  switch (kind) {
    case FUNCTION_KIND_COMPILED_FUNCTION:
      return "compiled function";
    case FUNCTION_KIND_MACRO:
      return "macro";
    case FUNCTION_KIND_SPECIAL_OPERATOR:
      return "special operator";
    default:
      return "function";
  }
}

const gchar *
function_get_name(const Function *function)
{
  return function ? function->name : NULL;
}

const gchar *
function_get_package(const Function *function)
{
  return function ? function->package : NULL;
}

gchar *
function_tooltip(Function *function)
{
  g_return_val_if_fail(function != NULL, NULL);
  GString *tt = g_string_new(NULL);
  const Node *lambda = function_get_lambda_list(function);
  const gchar *pkg = function_get_package(function);
  const gchar *name = function_get_name(function);
  if (lambda && name) {
    gchar *ls = node_to_string(lambda);
    gsize len = strlen(ls);
    FunctionKind kind = function_get_kind(function);
    const gchar *kind_name = function_get_kind_name(kind);
    gchar *pkg_esc = pkg ? g_markup_escape_text(pkg, -1) : NULL;
    gchar *name_esc = g_markup_escape_text(name, -1);
    if (name_esc) {
      if (pkg_esc)
        g_string_append_printf(tt,
            "<span foreground=\"brown\"><b>%s</b></span> is a %s in <span foreground=\"darkgreen\">%s</span>:\n",
            name_esc, kind_name, pkg_esc);
      else
        g_string_append_printf(tt,
            "<span foreground=\"brown\"><b>%s</b></span> is a %s:\n", name_esc,
            kind_name);
    }
    g_string_append_printf(tt, "(<span foreground=\"brown\"><b>%s</b></span>",
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
      g_string_append(tt, "\n\n");
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

