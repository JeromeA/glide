#include "function.h"
#include "project_file.h"

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
  function->symbol = symbol ? node_ref(symbol) : NULL;
  function->lambda_list = lambda_list ? node_ref(lambda_list) : NULL;
  function->doc_string = doc_string ? g_strdup(doc_string) : NULL;
  function->type = type ? node_ref(type) : NULL;
  function->name = name ? g_strdup(name) :
    (symbol ? g_strdup(node_get_name(symbol)) : NULL);
  function->package = package ? g_strdup(package) :
    (symbol ? g_strdup(symbol->package_context) : NULL);
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

