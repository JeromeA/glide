#include "project_repl.h"
#include "project.h"
#include "analyse_defpackage.h"
#include "analyse.h"
#include "document.h"
#include "repl_session.h"
#include "interaction.h"
#include "node.h"
#include "util.h"
#include <string.h>

struct _ProjectRepl {
  Project *project;
  ReplSession *session;
};

typedef struct {
  ProjectRepl *repl;
  Project *project;
} PackageRequestData;

static void project_on_package_definition(Interaction *interaction, gpointer user_data);
static void project_on_describe(Interaction *interaction, gpointer user_data);
static void project_handle_special_variable(Project *project,
    const gchar *package, const gchar *symbol, GPtrArray *section);
static void project_handle_function_section(Project *project,
    const gchar *package, const gchar *symbol, GPtrArray *section,
    FunctionKind kind);
static void project_repl_request_describe(ProjectRepl *self,
    const gchar *pkg_name, const gchar *symbol);

typedef struct {
  Project *project;
  gchar *package_name;
  gchar *symbol;
} DescribeData;

typedef struct {
  Project *project;
  Node *expr;
  Document *document;
} PackageDefinitionData;

typedef struct {
  Project *project;
  gchar *package;
  gchar *symbol;
  gchar *doc;
} VariableData;

typedef struct {
  Project *project;
  Function *function;
} FunctionData;

static gboolean analyse_defpackage_cb(gpointer data) {
  PackageDefinitionData *pd = data;
  AnalyseContext ctx = { g_strdup("CL-USER"), FALSE };
  analyse_defpackage(pd->project, pd->expr, &ctx);
  g_free(ctx.package);
  if (pd->document)
    document_free(pd->document);
  g_free(pd);
  return G_SOURCE_REMOVE;
}

static gboolean add_variable_cb(gpointer data) {
  VariableData *vd = data;
  project_add_variable(vd->project, vd->package, vd->symbol, vd->doc);
  g_free(vd->package);
  g_free(vd->symbol);
  g_free(vd->doc);
  g_free(vd);
  return G_SOURCE_REMOVE;
}

static gboolean add_function_cb(gpointer data) {
  FunctionData *fd = data;
  project_add_function(fd->project, fd->function);
  function_unref(fd->function);
  g_free(fd);
  return G_SOURCE_REMOVE;
}

static gboolean project_unref_cb(gpointer data) {
  project_unref(data);
  return G_SOURCE_REMOVE;
}

static void collect_export_symbols(Node *expr, GPtrArray *out) {
  if (!expr || !expr->children)
    return;
  for (guint i = 2; i < expr->children->len; i++) {
    Node *option = g_array_index(expr->children, Node*, i);
    if (!option || option->type != LISP_AST_NODE_TYPE_LIST || !option->children || option->children->len == 0)
      continue;
    Node *keyword_node = g_array_index(option->children, Node*, 0);
    const gchar *keyword = node_get_name(keyword_node);
    if (keyword && strcmp(keyword, "EXPORT") == 0) {
      for (guint j = 1; j < option->children->len; j++) {
        Node *sym = g_array_index(option->children, Node*, j);
        const gchar *name = node_get_name(sym);
        if (name)
          g_ptr_array_add(out, g_strdup(name));
      }
    }
  }
}

ProjectRepl *project_repl_new(Project *project, ReplSession *session) {
  g_return_val_if_fail(project != NULL, NULL);
  ProjectRepl *self = g_new0(ProjectRepl, 1);
  self->project = project;
  self->session = session ? repl_session_ref(session) : NULL;
  return self;
}

void project_repl_free(ProjectRepl *self) {
  if (!self)
    return;
  g_clear_pointer(&self->session, repl_session_unref);
  g_free(self);
}

void project_repl_request_package(ProjectRepl *self, const gchar *name) {
  g_return_if_fail(self);
  g_return_if_fail(self->session);
  g_return_if_fail(name);
  gchar *expr = g_strdup_printf("(glide:package-definition \"%s\")", name);
  Interaction *interaction = g_new0(Interaction, 1);
  interaction_init(interaction, expr);
  g_mutex_lock(&interaction->lock);
  interaction->type = INTERACTION_INTERNAL;
  interaction->done_cb = project_on_package_definition;
  PackageRequestData *data = g_new0(PackageRequestData, 1);
  data->repl = self;
  data->project = project_ref(self->project);
  interaction->done_cb_data = data;
  g_mutex_unlock(&interaction->lock);
  repl_session_eval(self->session, interaction);
  g_free(expr);
}

static void project_repl_request_describe(ProjectRepl *self,
    const gchar *pkg_name, const gchar *symbol) {
  g_return_if_fail(self);
  g_return_if_fail(self->session);
  g_return_if_fail(pkg_name);
  g_return_if_fail(symbol);
  LOG(1, "project_request_describe pkg=%s symbol=%s", pkg_name, symbol);
  gchar *expr = g_strdup_printf("(describe '%s:%s)", pkg_name, symbol);
  Interaction *interaction = g_new0(Interaction, 1);
  interaction_init(interaction, expr);
  g_mutex_lock(&interaction->lock);
  interaction->type = INTERACTION_INTERNAL;
  DescribeData *data = g_new0(DescribeData, 1);
  data->project = project_ref(self->project);
  data->package_name = g_strdup(pkg_name);
  data->symbol = g_strdup(symbol);
  interaction->done_cb = project_on_describe;
  interaction->done_cb_data = data;
  g_mutex_unlock(&interaction->lock);
  repl_session_eval(self->session, interaction);
  g_free(expr);
}

static void project_on_package_definition(Interaction *interaction, gpointer user_data) {
  LOG(1, "project_on_package_definition entry");
  PackageRequestData *data = user_data;
  Project *project = data->project;
  gchar *res = NULL;
  g_mutex_lock(&interaction->lock);
  if (interaction->result)
    res = g_strdup(interaction->result->str);
  g_mutex_unlock(&interaction->lock);
  g_assert(res);
  GString *text = g_string_new(res);
  Document *document = document_new(NULL, DOCUMENT_DORMANT);
  document_set_content(document, g_string_new(text->str));
  Node *ast = (Node*)document_get_ast(document);
  g_assert(ast && ast->children && ast->children->len > 0);
  Node *expr = g_array_index(ast->children, Node*, 0);
  Node *name_node = (expr->children && expr->children->len > 1) ? g_array_index(expr->children, Node*, 1) : NULL;
  gchar *pkg_name = g_strdup(node_get_name(name_node));
  g_assert(pkg_name);
  LOG(1, "project_on_package_definition built package %s", pkg_name);
  GPtrArray *exports = g_ptr_array_new_with_free_func(g_free);
  collect_export_symbols(expr, exports);
  PackageDefinitionData *pd = g_new0(PackageDefinitionData, 1);
  pd->project = project;
  pd->expr = expr;
  pd->document = document;
  g_main_context_invoke(NULL, analyse_defpackage_cb, pd);
  for (guint i = 0; i < exports->len; i++) {
    const gchar *sym = g_ptr_array_index(exports, i);
    project_repl_request_describe(data->repl, pkg_name, sym);
  }
  g_free(pkg_name);
  g_main_context_invoke(NULL, project_unref_cb, project);
  g_ptr_array_free(exports, TRUE);
  interaction_clear(interaction);
  g_free(interaction);
  g_free(res);
  g_string_free(text, TRUE);
  g_free(data);
}

static void project_handle_special_variable(Project *project,
    const gchar *package, const gchar *symbol, GPtrArray *section) {
  LOG(1, "project_handle_special_variable symbol=%s", symbol);
  gchar *declared_type = NULL;
  gchar *value = NULL;
  GString *doc = NULL;
  for (guint i = 1; i < section->len; i++) {
    const gchar *line = g_ptr_array_index(section, i);
    if (g_str_has_prefix(line, "  Declared type:")) {
      g_free(declared_type);
      declared_type = g_strdup(g_strstrip((gchar*)line + 15));
    } else if (g_str_has_prefix(line, "  Value:")) {
      g_free(value);
      value = g_strdup(g_strstrip((gchar*)line + 7));
    } else if (g_str_has_prefix(line, "  Documentation:")) {
      if (!doc)
        doc = g_string_new(NULL);
      for (guint j = i + 1; j < section->len; j++) {
        const gchar *dline = g_ptr_array_index(section, j);
        if (g_str_has_prefix(dline, "    ")) {
          if (doc->len)
            g_string_append_c(doc, '\n');
          g_string_append(doc, g_strstrip((gchar*)dline + 4));
        } else {
          break;
        }
      }
    }
  }
  LOG(1, "describe %s special variable type=%s value=%s", symbol,
      declared_type ? declared_type : "(unknown)",
      value ? value : "(unknown)");
  LOG_LONG(1, "↳ doc: ", doc ? doc->str : "");
  VariableData *vd = g_new0(VariableData, 1);
  vd->project = project;
  vd->package = g_strdup(package);
  vd->symbol = g_strdup(symbol);
  vd->doc = doc ? g_strdup(doc->str) : NULL;
  g_main_context_invoke(NULL, add_variable_cb, vd);
  g_free(declared_type);
  g_free(value);
  if (doc)
    g_string_free(doc, TRUE);
}

static void project_handle_function_section(Project *project,
    const gchar *package, const gchar *symbol, GPtrArray *section,
    FunctionKind kind) {
  const gchar *log_label = "function";
  const gchar *kind_name = "function";
  switch (kind) {
    case FUNCTION_KIND_COMPILED_FUNCTION:
      log_label = "compiled_function";
      kind_name = "compiled function";
      break;
    case FUNCTION_KIND_MACRO:
      log_label = "macro";
      kind_name = "macro";
      break;
    case FUNCTION_KIND_SPECIAL_OPERATOR:
      log_label = "special_operator";
      kind_name = "special operator";
      break;
    default:
      break;
  }
  LOG(1, "project_handle_%s symbol=%s", log_label, symbol);
  gchar *lambda_list = NULL;
  GString *doc = NULL;
  for (guint i = 1; i < section->len; i++) {
    const gchar *line = g_ptr_array_index(section, i);
    if (g_str_has_prefix(line, "  Lambda-list:")) {
      g_free(lambda_list);
      lambda_list = g_strdup(g_strstrip((gchar*)line + 14));
    } else if (g_str_has_prefix(line, "  Documentation:")) {
      if (!doc)
        doc = g_string_new(NULL);
      for (guint j = i + 1; j < section->len; j++) {
        const gchar *dline = g_ptr_array_index(section, j);
        if (g_str_has_prefix(dline, "    ")) {
          if (doc->len)
            g_string_append_c(doc, '\n');
          g_string_append(doc, g_strstrip((gchar*)dline + 4));
        } else {
          break;
        }
      }
    }
  }
  LOG(1, "describe %s %s lambda=%s", symbol,
      kind_name, lambda_list ? lambda_list : "(unknown)");
  LOG_LONG(1, "↳ doc: ", doc ? doc->str : "");
  Document *document = NULL;
  Node *lambda_node = NULL;
  if (lambda_list) {
    document = document_new(NULL, DOCUMENT_LIVE);
    document_set_content(document, g_string_new(lambda_list));
    const Node *ast = document_get_ast(document);
    if (ast && ast->children && ast->children->len > 0)
      lambda_node = g_array_index(ast->children, Node*, 0);
  }
  Function *function = function_new(NULL, lambda_node, doc ? doc->str : NULL,
      NULL, kind, symbol, package, document);
  FunctionData *fd = g_new0(FunctionData, 1);
  fd->project = project;
  fd->function = function;
  g_main_context_invoke(NULL, add_function_cb, fd);
  g_free(lambda_list);
  if (doc)
    g_string_free(doc, TRUE);
}

static void project_on_describe(Interaction *interaction, gpointer user_data) {
  DescribeData *data = user_data;
  LOG(1, "project_on_describe symbol=%s", data->symbol);
  gchar *out = NULL;
  g_mutex_lock(&interaction->lock);
  if (interaction->output)
    out = g_strdup(interaction->output->str);
  g_mutex_unlock(&interaction->lock);
  g_return_if_fail(out);
  gchar **lines = g_strsplit(out, "\n", -1);
  GPtrArray *sections = g_ptr_array_new_with_free_func((GDestroyNotify)g_ptr_array_unref);
  GPtrArray *current = NULL;
  for (guint i = 0; lines[i]; i++) {
    if (lines[i][0] != ' ' && lines[i][0] != '\0') {
      current = g_ptr_array_new_with_free_func(g_free);
      g_ptr_array_add(sections, current);
    }
    if (current)
      g_ptr_array_add(current, g_strdup(lines[i]));
  }
  g_strfreev(lines);
  for (guint s = 1; s < sections->len; s++) {
    GPtrArray *section = g_ptr_array_index(sections, s);
    if (section->len == 0)
      continue;
    const gchar *first_line = g_ptr_array_index(section, 0);
    LOG(1, "describe %s section: %s", data->symbol, first_line);
    if (g_str_has_suffix(first_line, "names a special variable:")) {
      project_handle_special_variable(data->project, data->package_name,
          data->symbol, section);
    } else if (g_str_has_suffix(first_line, "names a compiled function:")) {
      project_handle_function_section(data->project, data->package_name,
          data->symbol, section, FUNCTION_KIND_COMPILED_FUNCTION);
    } else if (g_str_has_suffix(first_line, "names a macro:")) {
      project_handle_function_section(data->project, data->package_name,
          data->symbol, section, FUNCTION_KIND_MACRO);
    } else if (g_str_has_suffix(first_line, "names a special operator:")) {
      project_handle_function_section(data->project, data->package_name,
          data->symbol, section, FUNCTION_KIND_SPECIAL_OPERATOR);
    } else {
      LOG(1, "describe %s ignoring section: %s", data->symbol, first_line);
    }
  }
  g_ptr_array_free(sections, TRUE);
  g_main_context_invoke(NULL, project_unref_cb, data->project);
  g_free(data->package_name);
  g_free(data->symbol);
  g_free(data);
  interaction_clear(interaction);
  g_free(interaction);
  g_free(out);
}

