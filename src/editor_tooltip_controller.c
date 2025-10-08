#include "editor_tooltip_controller.h"

#include "document.h"
#include "editor.h"
#include "function.h"
#include "node.h"
#include "project.h"
#include "util.h"

struct _EditorTooltipController
{
  EditorTooltipWindow *window;
  Project *project;
};

static gboolean editor_tooltip_controller_ensure_window(EditorTooltipController *self, GtkWidget *view);
static const Node *editor_tooltip_controller_find_sdt_node(Document *document, gsize offset);
static gchar *editor_tooltip_controller_build_function_markup(Document *document, Project *project,
    gsize offset);
static gchar *editor_tooltip_controller_build_error_markup(Document *document, gsize offset);

EditorTooltipController *
editor_tooltip_controller_new(GtkWidget *view, Project *project)
{
  g_return_val_if_fail(project != NULL, NULL);

  EditorTooltipController *self = g_new0(EditorTooltipController, 1);
  if (!self)
    return NULL;

  self->project = project_ref(project);

  if (view)
    editor_tooltip_controller_ensure_window(self, view);

  return self;
}

void
editor_tooltip_controller_free(EditorTooltipController *self)
{
  if (!self)
    return;

  if (self->project) {
    project_unref(self->project);
    self->project = NULL;
  }
  g_clear_object(&self->window);
  g_free(self);
}

static gboolean
editor_tooltip_controller_ensure_window(EditorTooltipController *self, GtkWidget *view)
{
  g_return_val_if_fail(self != NULL, FALSE);

  if (self->window)
    return TRUE;

  if (!view || !GTK_IS_WIDGET(view))
    return FALSE;

  self->window = editor_tooltip_window_new();
  if (!self->window)
    return FALSE;

  g_object_ref_sink(G_OBJECT(self->window));
  gtk_widget_set_tooltip_window(view, GTK_WINDOW(self->window));
  return TRUE;
}

static const Node *
editor_tooltip_controller_find_sdt_node(Document *document, gsize offset)
{
  g_return_val_if_fail(document != NULL, NULL);

  const Node *ast = document_get_ast(document);
  if (!ast)
    return NULL;

  return node_find_sdt_containing_offset(ast, offset);
}

static gchar *
editor_tooltip_controller_build_function_markup(Document *document, Project *project, gsize offset)
{
  const Node *node = editor_tooltip_controller_find_sdt_node(document, offset);
  if (!node) {
    LOG(2, "EditorTooltipController.build_function_markup: no node");
    return NULL;
  }

  gchar *node_str = node_to_string(node);
  LOG(2, "EditorTooltipController.build_function_markup: node %s", node_str ? node_str : "<unknown>");
  g_free(node_str);

  if (!node_is(node, SDT_FUNCTION_USE)) {
    LOG(2, "EditorTooltipController.build_function_markup: node not a function use");
    return NULL;
  }

  const gchar *name = node_get_name(node);
  LOG(2, "EditorTooltipController.build_function_markup: function %s", name ? name : "(null)");

  if (!project) {
    LOG(1, "EditorTooltipController.build_function_markup: project unavailable");
    return NULL;
  }

  Function *fn = project_get_function(project, name);
  if (!fn) {
    LOG(1, "EditorTooltipController.build_function_markup: function not found");
    return NULL;
  }

  gchar *markup = function_tooltip(fn);
  if (!markup)
    LOG(1, "EditorTooltipController.build_function_markup: no tooltip");

  return markup;
}

static gchar *
editor_tooltip_controller_build_error_markup(Document *document, gsize offset)
{
  if (!document)
    return NULL;

  const GArray *errors = document_get_errors(document);
  if (!errors || errors->len == 0)
    return NULL;

  LOG(2, "EditorTooltipController.build_error_markup checking %u errors", errors->len);
  for (guint i = 0; i < errors->len; i++) {
    const DocumentError *err = &g_array_index((GArray *) errors, DocumentError, i);
    if (offset < err->start || offset >= err->end)
      continue;

    LOG(1, "EditorTooltipController.build_error_markup: match range=[%zu,%zu) message=%s",
        err->start, err->end, err->message ? err->message : "(null)");
    const gchar *message = (err->message && *err->message) ? err->message : "Error";
    return g_markup_escape_text(message, -1);
  }

  LOG(2, "EditorTooltipController.build_error_markup: no match at offset %zu", offset);
  return NULL;
}

gboolean
editor_tooltip_controller_query(EditorTooltipController *self, Editor *editor, GtkWidget *widget, gint x, gint y)
{
  g_assert(glide_is_ui_thread());
  g_return_val_if_fail(self != NULL, FALSE);
  g_return_val_if_fail(GLIDE_IS_EDITOR(editor), FALSE);
  g_return_val_if_fail(GTK_IS_WIDGET(widget), FALSE);

  Document *document = editor_get_document(editor);
  if (!document)
    return FALSE;

  if (!self->project)
    return FALSE;

  if (!editor_tooltip_controller_ensure_window(self, widget))
    return FALSE;

  GtkTextIter iter;
  gint bx;
  gint by;
  gtk_text_view_window_to_buffer_coords(GTK_TEXT_VIEW(widget), GTK_TEXT_WINDOW_WIDGET, x, y, &bx, &by);
  gtk_text_view_get_iter_at_location(GTK_TEXT_VIEW(widget), &iter, bx, by);
  gsize offset = gtk_text_iter_get_offset(&iter);
  LOG(2, "EditorTooltipController.query offset=%zu", offset);

  gchar *error_markup = editor_tooltip_controller_build_error_markup(document, offset);
  gchar *function_markup = editor_tooltip_controller_build_function_markup(document, self->project, offset);

  gboolean shown = FALSE;
  if (self->window)
    shown = editor_tooltip_window_set_content(self->window, error_markup, function_markup);

  g_free(function_markup);
  g_free(error_markup);

  return shown;
}

gboolean
editor_tooltip_controller_show(EditorTooltipController *self)
{
  g_assert(glide_is_ui_thread());
  g_return_val_if_fail(self != NULL, FALSE);

  if (!self->window) {
    LOG(1, "EditorTooltipController.show: no tooltip window");
    return FALSE;
  }

  if (!editor_tooltip_window_has_content(self->window)) {
    LOG(1, "EditorTooltipController.show: no cached tooltip content");
    return FALSE;
  }

  gtk_widget_show(GTK_WIDGET(self->window));
  gtk_window_present(GTK_WINDOW(self->window));

  return TRUE;
}

