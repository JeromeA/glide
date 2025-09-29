#include "editor.h"
#include "editor_tooltip_window.h"
#include "project.h"
#include "document.h"
#include "util.h"

typedef struct {
  gsize start;
  gsize end;
} SelectionRange;

struct _Editor
{
  GtkScrolledWindow parent_instance;

  GtkSourceView *view;
  GtkSourceBuffer *buffer;
  Project *project;
  Document *document;
  GArray *selection_stack;
  GtkTextTag *function_def_tag;
  GtkTextTag *function_use_tag;
  GtkTextTag *error_tag;
  EditorTooltipWindow *tooltip_window;
};

G_DEFINE_TYPE (Editor, editor, GTK_TYPE_SCROLLED_WINDOW)

// Forward declaration for the callback
static void on_buffer_changed (GtkTextBuffer *buffer, gpointer user_data);
static gboolean editor_on_query_tooltip (GtkWidget *widget, gint x, gint y,
    gboolean /*keyboard_mode*/, GtkTooltip * /*tooltip*/, gpointer user_data);
static void editor_update_function_highlight (Editor *self);
static const Node *editor_find_sdt_node (Editor *self, gsize offset);
static void editor_on_mark_set (GtkTextBuffer *buffer, GtkTextIter * /*location*/,
    GtkTextMark *mark, gpointer user_data);
static void editor_clear_function_highlight (Editor *self);
static void editor_highlight_nodes (Editor *self, GPtrArray *nodes,
    GtkTextTag *tag);
static void editor_highlight_node (Editor *self, const Node *node,
    GtkTextTag *tag);
static gchar *editor_build_error_tooltip_markup (Editor *self, gsize offset);
static void editor_clear_errors(Editor *self);
static void editor_update_document_from_buffer(Editor *self);

static void
editor_init (Editor *self)
{
  GtkSourceLanguageManager *lm = gtk_source_language_manager_get_default ();
  GtkSourceLanguage *lang = gtk_source_language_manager_get_language (lm, "commonlisp");
  self->buffer = gtk_source_buffer_new_with_language (lang);
  self->view = GTK_SOURCE_VIEW (gtk_source_view_new ());
  gtk_text_view_set_buffer (GTK_TEXT_VIEW (self->view), GTK_TEXT_BUFFER (self->buffer));
  gtk_source_view_set_show_line_numbers (self->view, TRUE);
  gtk_text_view_set_monospace (GTK_TEXT_VIEW (self->view), TRUE);
  gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (self),
      GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
  gtk_container_add (GTK_CONTAINER (self), GTK_WIDGET (self->view));
  gtk_widget_set_has_tooltip (GTK_WIDGET (self->view), TRUE);
  g_signal_connect (self->view, "query-tooltip", G_CALLBACK (editor_on_query_tooltip), self);

  self->project = NULL;
  self->document = NULL;
  self->selection_stack = g_array_new (FALSE, FALSE, sizeof (SelectionRange));
  self->tooltip_window = editor_tooltip_window_new ();
  if (self->tooltip_window) {
    g_object_ref_sink (G_OBJECT (self->tooltip_window));
    gtk_widget_set_tooltip_window (GTK_WIDGET (self->view),
        GTK_WINDOW (self->tooltip_window));
  }
  GtkTextBuffer *buffer = GTK_TEXT_BUFFER (self->buffer);
  self->function_def_tag = gtk_text_buffer_create_tag (buffer,
      "function-def-highlight", "background", "#fef", NULL);
  self->function_use_tag = gtk_text_buffer_create_tag (buffer,
      "function-use-highlight", "background", "#eef", NULL);
  self->error_tag = gtk_text_buffer_create_tag(buffer,
      "error-highlight", "underline", PANGO_UNDERLINE_ERROR, NULL);
  g_signal_connect (buffer, "mark-set", G_CALLBACK (editor_on_mark_set), self);
}

// Callback for when the GtkTextBuffer changes
static void
on_buffer_changed(GtkTextBuffer * /*buffer*/, gpointer user_data)
{
  Editor *self = GLIDE_EDITOR(user_data);
  if (!self) {
    g_message("Editor buffer change callback invoked with NULL editor");
    return;
  }

  editor_update_document_from_buffer(self);
  editor_set_errors(self, document_get_errors(self->document));
  editor_update_function_highlight(self);
}

static void
editor_dispose (GObject *object)
{
  Editor *self = GLIDE_EDITOR (object);

  if (self->buffer)
    g_signal_handlers_disconnect_by_data (self->buffer, self);

  if (self->project) {
    project_unref (self->project);
    self->project = NULL;
  }

  self->function_def_tag = NULL;
  self->function_use_tag = NULL;
  self->error_tag = NULL;
  g_clear_object (&self->buffer);
  self->view = NULL;
  if (self->selection_stack) {
    g_array_free (self->selection_stack, TRUE);
    self->selection_stack = NULL;
  }
  g_clear_object (&self->tooltip_window);

  G_OBJECT_CLASS (editor_parent_class)->dispose (object);
}

static void
editor_class_init (EditorClass *klass)
{
  GObjectClass *object_class = G_OBJECT_CLASS (klass);
  object_class->dispose = editor_dispose;
}

GtkWidget *
editor_new_for_document (Project *project, Document *document)
{
  g_return_val_if_fail(project != NULL, NULL);
  g_return_val_if_fail(document != NULL, NULL);

  Editor *self = g_object_new (EDITOR_TYPE, NULL);
  self->project = project_ref(project);
  self->document = document;

  const GString *existing = document_get_content(self->document);
  if (existing && existing->str) {
    gtk_source_buffer_begin_not_undoable_action(self->buffer);
    gtk_text_buffer_set_text(GTK_TEXT_BUFFER(self->buffer), existing->str, -1);
    gtk_source_buffer_end_not_undoable_action(self->buffer);
  }

  project_document_changed (self->project, self->document);
  gtk_text_buffer_set_modified (GTK_TEXT_BUFFER (self->buffer), FALSE);
  g_signal_connect (self->buffer, "changed", G_CALLBACK (on_buffer_changed), self);
  editor_update_function_highlight (self);
  return GTK_WIDGET (self);
}


GtkSourceBuffer *
editor_get_buffer (Editor *self)
{
  g_return_val_if_fail (GLIDE_IS_EDITOR (self), NULL);
  return self->buffer;
}

Document *
editor_get_document(Editor *self)
{
  g_return_val_if_fail(GLIDE_IS_EDITOR(self), NULL);
  return self->document;
}

GtkWidget *
editor_get_view (Editor *self)
{
  g_return_val_if_fail (GLIDE_IS_EDITOR (self), NULL);
  return GTK_WIDGET (self->view);
}

static void
editor_clear_function_highlight (Editor *self)
{
  if (!self->buffer)
    return;
  GtkTextBuffer *buffer = GTK_TEXT_BUFFER (self->buffer);
  GtkTextIter start;
  GtkTextIter end;
  gtk_text_buffer_get_bounds (buffer, &start, &end);
  if (self->function_def_tag)
    gtk_text_buffer_remove_tag (buffer, self->function_def_tag, &start, &end);
  if (self->function_use_tag)
    gtk_text_buffer_remove_tag (buffer, self->function_use_tag, &start, &end);
}

static void
editor_clear_errors(Editor *self)
{
  if (!self || !self->buffer || !self->error_tag)
    return;
  GtkTextBuffer *buffer = GTK_TEXT_BUFFER(self->buffer);
  GtkTextIter start;
  GtkTextIter end;
  gtk_text_buffer_get_bounds(buffer, &start, &end);
  gtk_text_buffer_remove_tag(buffer, self->error_tag, &start, &end);
}

static void
editor_update_document_from_buffer(Editor *self)
{
  g_return_if_fail(GLIDE_IS_EDITOR(self));
  g_return_if_fail(self->buffer != NULL);

  GtkTextBuffer *buffer = GTK_TEXT_BUFFER(self->buffer);
  GtkTextIter start;
  GtkTextIter end;
  gtk_text_buffer_get_start_iter(buffer, &start);
  gtk_text_buffer_get_end_iter(buffer, &end);
  gchar *text = gtk_text_buffer_get_text(buffer, &start, &end, FALSE);
  GString *content = g_string_new(text ? text : "");
  g_free(text);
  document_set_content(self->document, content);
}

static void
editor_highlight_nodes (Editor *self, GPtrArray *nodes, GtkTextTag *tag)
{
  if (!nodes || !tag)
    return;
  for (guint i = 0; i < nodes->len; i++) {
    Node *node = g_ptr_array_index (nodes, i);
    editor_highlight_node (self, node, tag);
  }
}

void
editor_set_errors(Editor *self, const GArray *errors)
{
  g_return_if_fail(GLIDE_IS_EDITOR(self));
  editor_clear_errors(self);
  if (!errors || !self->buffer || !self->error_tag)
    return;
  GtkTextBuffer *buffer = GTK_TEXT_BUFFER(self->buffer);
  for (guint i = 0; i < errors->len; i++) {
    const DocumentError *err = &g_array_index(errors, DocumentError, i);
    GtkTextIter start;
    GtkTextIter end;
    gtk_text_buffer_get_iter_at_offset(buffer, &start, (gint)err->start);
    gtk_text_buffer_get_iter_at_offset(buffer, &end, (gint)err->end);
    gtk_text_buffer_apply_tag(buffer, self->error_tag, &start, &end);
  }
}

static void
editor_highlight_node (Editor *self, const Node *node, GtkTextTag *tag)
{
  if (!node || !tag)
    return;
  if (node->document != self->document)
    return;
  const Node *highlight = node_get_symbol_name_node_const (node);
  if (!highlight)
    highlight = node;
  gsize start = node_get_start_offset (highlight);
  gsize end = node_get_end_offset (highlight);
  if (end <= start)
    return;
  GtkTextIter it_start;
  GtkTextIter it_end;
  GtkTextBuffer *buffer = GTK_TEXT_BUFFER (self->buffer);
  gtk_text_buffer_get_iter_at_offset (buffer, &it_start, (gint) start);
  gtk_text_buffer_get_iter_at_offset (buffer, &it_end, (gint) end);
  gtk_text_buffer_apply_tag (buffer, tag, &it_start, &it_end);
}

static const Node *
editor_find_sdt_node (Editor *self, gsize offset)
{
  g_return_val_if_fail (GLIDE_IS_EDITOR (self), NULL);
  if (!self->document)
    return NULL;

  const Node *ast = document_get_ast (self->document);
  if (!ast)
    return NULL;

  return node_find_sdt_containing_offset (ast, offset);
}

static void
editor_update_function_highlight (Editor *self)
{
  g_return_if_fail (GLIDE_IS_EDITOR (self));
  editor_clear_function_highlight (self);
  if (!self->project || !self->document)
    return;
  GtkTextBuffer *buffer = GTK_TEXT_BUFFER (self->buffer);
  GtkTextMark *insert = gtk_text_buffer_get_insert (buffer);
  GtkTextIter iter;
  gtk_text_buffer_get_iter_at_mark (buffer, &iter, insert);
  gsize offset = gtk_text_iter_get_offset (&iter);
  const Node *node = editor_find_sdt_node (self, offset);
  while (node && node->sd_type != SDT_FUNCTION_DEF && node->sd_type != SDT_FUNCTION_USE)
    node = node->parent;
  if (!node)
    return;
  const gchar *name = node_get_name (node);
  if (!name)
    return;
  GHashTable *def_table = project_get_index (self->project, SDT_FUNCTION_DEF);
  GHashTable *use_table = project_get_index (self->project, SDT_FUNCTION_USE);
  GPtrArray *defs = def_table ? g_hash_table_lookup (def_table, name) : NULL;
  GPtrArray *uses = use_table ? g_hash_table_lookup (use_table, name) : NULL;
  if (!defs && node->sd_type == SDT_FUNCTION_DEF)
    editor_highlight_node (self, node, self->function_def_tag);
  else
    editor_highlight_nodes (self, defs, self->function_def_tag);
  if (!uses && node->sd_type == SDT_FUNCTION_USE)
    editor_highlight_node (self, node, self->function_use_tag);
  else
    editor_highlight_nodes (self, uses, self->function_use_tag);
}

static void
editor_on_mark_set (GtkTextBuffer *buffer, GtkTextIter * /*location*/,
    GtkTextMark *mark, gpointer user_data)
{
  Editor *self = GLIDE_EDITOR (user_data);
  if (mark != gtk_text_buffer_get_insert (buffer))
    return;
  editor_update_function_highlight (self);
}

static gboolean find_parent_range (GtkTextBuffer *buffer, Document *document,
    gsize start, gsize end, gsize *new_start, gsize *new_end);
static gchar *editor_build_function_tooltip_markup (Editor *self, gsize offset);

gboolean
editor_get_toplevel_range (Editor *self, gsize offset,
    gsize *start, gsize *end)
{
  g_return_val_if_fail (GLIDE_IS_EDITOR (self), FALSE);
  g_return_val_if_fail (start != NULL, FALSE);
  g_return_val_if_fail (end != NULL, FALSE);
  GtkTextBuffer *buffer = GTK_TEXT_BUFFER(self->buffer);
  if (!self->document)
    return FALSE;

  GtkTextIter end_iter;
  gtk_text_buffer_get_end_iter(buffer, &end_iter);
  gsize len = gtk_text_iter_get_offset(&end_iter);

  gsize cur_start = offset;
  gsize cur_end = offset;
  gsize new_start;
  gsize new_end;

  while (find_parent_range(buffer, self->document, cur_start, cur_end, &new_start, &new_end)) {
    cur_start = new_start;
    cur_end = new_end;
    if (cur_start == 0 && cur_end == len)
      break;
  }

  if (cur_start == offset && cur_end == offset)
    return FALSE;

  *start = cur_start;
  *end = cur_end;
  return TRUE;
}

static gboolean
editor_on_query_tooltip (GtkWidget *widget, gint x, gint y, gboolean /*keyboard_mode*/,
    GtkTooltip * /*tooltip*/, gpointer user_data)
{
  g_assert (glide_is_ui_thread ());
  Editor *self = GLIDE_EDITOR (user_data);
  g_return_val_if_fail (self != NULL, FALSE);
  g_return_val_if_fail (self->document != NULL, FALSE);
  g_return_val_if_fail (self->project != NULL, FALSE);
  GtkTextIter iter;
  gint bx;
  gint by;
  gtk_text_view_window_to_buffer_coords (GTK_TEXT_VIEW (widget), GTK_TEXT_WINDOW_WIDGET, x, y, &bx, &by);
  gtk_text_view_get_iter_at_location (GTK_TEXT_VIEW (widget), &iter, bx, by);
  gsize offset = gtk_text_iter_get_offset (&iter);
  LOG (2, "Editor.on_query_tooltip offset=%zu", offset);

  gchar *error_markup = editor_build_error_tooltip_markup (self, offset);
  gchar *function_markup = editor_build_function_tooltip_markup (self, offset);
  gboolean shown = FALSE;

  if (!self->tooltip_window) {
    self->tooltip_window = editor_tooltip_window_new ();
    if (self->tooltip_window) {
      g_object_ref_sink (G_OBJECT (self->tooltip_window));
      gtk_widget_set_tooltip_window (GTK_WIDGET (self->view),
          GTK_WINDOW (self->tooltip_window));
    }
  }

  if (self->tooltip_window) {
    if (editor_tooltip_window_set_content (self->tooltip_window, error_markup, function_markup))
      shown = TRUE;
  }

  g_free (function_markup);
  g_free (error_markup);
  return shown;
}

static gchar *
editor_build_function_tooltip_markup (Editor *self, gsize offset)
{
  const Node *node = editor_find_sdt_node (self, offset);
  if (!node) {
    LOG (2, "Editor.build_function_tooltip_markup: no node");
    return NULL;
  }

  gchar *node_str = node_to_string (node);
  LOG (2, "Editor.build_function_tooltip_markup: node %s",
      node_str ? node_str : "<unknown>");
  g_free (node_str);

  if (!node_is (node, SDT_FUNCTION_USE)) {
    LOG (2, "Editor.build_function_tooltip_markup: node not a function use");
    return NULL;
  }

  const gchar *name = node_get_name (node);
  LOG (2, "Editor.build_function_tooltip_markup: function %s", name);

  Function *fn = project_get_function (self->project, name);
  if (!fn) {
    LOG (1, "Editor.build_function_tooltip_markup: function not found");
    return NULL;
  }

  gchar *markup = function_tooltip (fn);
  if (!markup)
    LOG (1, "Editor.build_function_tooltip_markup: no tooltip");

  return markup;
}

static gchar *
editor_build_error_tooltip_markup (Editor *self, gsize offset)
{
  const GArray *errors = document_get_errors (self->document);
  if (!errors || errors->len == 0)
    return NULL;
  LOG (2, "Editor.build_error_tooltip checking %u errors", errors->len);
  for (guint i = 0; i < errors->len; i++) {
    const DocumentError *err = &g_array_index ((GArray*) errors,
        DocumentError, i);
    if (offset < err->start || offset >= err->end)
      continue;
    LOG (1, "Editor.build_error_tooltip: match range=[%zu,%zu) message=%s",
        err->start, err->end, err->message ? err->message : "(null)");
    const gchar *message = (err->message && *err->message) ? err->message : "Error";
    gchar *message_esc = g_markup_escape_text (message, -1);
    return message_esc;
  }
  LOG (2, "Editor.build_error_tooltip: no match at offset %zu", offset);
  return NULL;
}

static gboolean
find_parent_range (GtkTextBuffer *buffer, Document *document,
    gsize start, gsize end, gsize *new_start, gsize *new_end)
{
  GtkTextIter end_iter;
  gtk_text_buffer_get_end_iter (buffer, &end_iter);
  gsize len = gtk_text_iter_get_offset (&end_iter);

  LOG(1, "find_parent_range start=%zu end=%zu", start, end);

  const Node *ast = document_get_ast (document);
  const Node *node = node_find_containing_range (ast, start, end);
  if (!node) {
    LOG(1, "no node found");
    return FALSE;
  }

  gsize node_start = node_get_start_offset (node);
  gsize node_end = node_get_end_offset (node);
  const Node *parent = node->parent;

  while (node_start == start && node_end == end && parent) {
    LOG(1, "parent has same range [%zu,%zu), climbing", node_start, node_end);
    node = parent;
    node_start = node_get_start_offset (node);
    node_end = node_get_end_offset (node);
    parent = node->parent;
  }

  if (!parent) {
    LOG(1, "at root node, cannot extend");
    return FALSE;
  }

  if (node_start == start && node_end == end) {
    if (start == 0 && end == len) {
      LOG(1, "at buffer range, cannot extend");
      return FALSE;
    }
    *new_start = 0;
    *new_end = len;
    LOG(1, "extending to buffer [%zu,%zu)", *new_start, *new_end);
    return TRUE;
  }

  *new_start = node_start;
  *new_end = node_end;
  LOG(1, "new range [%zu,%zu)", *new_start, *new_end);
  return TRUE;
}

static void select_range (GtkTextBuffer *buffer, gsize start, gsize end)
{
  GtkTextIter it_start;
  GtkTextIter it_end;
  gtk_text_buffer_get_iter_at_offset (buffer, &it_start, start);
  gtk_text_buffer_get_iter_at_offset (buffer, &it_end, end);
  gtk_text_buffer_select_range (buffer, &it_start, &it_end);
}

void
editor_extend_selection (Editor *self)
{
  g_return_if_fail (GLIDE_IS_EDITOR (self));
  GtkTextBuffer *buffer = GTK_TEXT_BUFFER (self->buffer);
  GtkTextIter it_start;
  GtkTextIter it_end;
  if (!gtk_text_buffer_get_selection_bounds (buffer, &it_start, &it_end)) {
    GtkTextMark *mark = gtk_text_buffer_get_insert (buffer);
    gtk_text_buffer_get_iter_at_mark (buffer, &it_start, mark);
    it_end = it_start;
  }
  gsize start = gtk_text_iter_get_offset (&it_start);
  gsize end = gtk_text_iter_get_offset (&it_end);

  LOG(1, "extend_selection start=%zu end=%zu stack_len=%u", start, end,
      self->selection_stack->len);

  if (self->selection_stack->len == 0) {
    SelectionRange original = { start, end };
    g_array_append_val (self->selection_stack, original);
  }

  gsize new_start = start;
  gsize new_end = end;
  if (!find_parent_range (buffer, self->document, start, end, &new_start, &new_end)) {
    LOG(1, "no parent range found, resetting");
    SelectionRange orig = g_array_index (self->selection_stack, SelectionRange, 0);
    select_range (buffer, orig.start, orig.end);
    g_array_set_size (self->selection_stack, 0);
    return;
  }

  SelectionRange new_range = { new_start, new_end };
  g_array_append_val (self->selection_stack, new_range);
  select_range (buffer, new_start, new_end);
  LOG(1, "selection extended to [%zu,%zu)", new_start, new_end);
}

void
editor_shrink_selection (Editor *self)
{
  g_return_if_fail (GLIDE_IS_EDITOR (self));
  if (!self->selection_stack || self->selection_stack->len == 0)
    return;

  GtkTextBuffer *buffer = GTK_TEXT_BUFFER (self->buffer);
  if (self->selection_stack->len <= 1) {
    SelectionRange orig = g_array_index (self->selection_stack, SelectionRange, 0);
    select_range (buffer, orig.start, orig.end);
    g_array_set_size (self->selection_stack, 0);
    return;
  }

  g_array_remove_index (self->selection_stack, self->selection_stack->len - 1);
  SelectionRange prev = g_array_index (self->selection_stack, SelectionRange,
      self->selection_stack->len - 1);
  select_range (buffer, prev.start, prev.end);
}

gboolean
editor_show_tooltip_window (Editor *self)
{
  g_assert (glide_is_ui_thread ());
  g_return_val_if_fail (GLIDE_IS_EDITOR (self), FALSE);

  if (!self->tooltip_window) {
    LOG (1, "Editor.show_tooltip_window: no tooltip window");
    return FALSE;
  }

  if (!editor_tooltip_window_has_content (self->tooltip_window)) {
    LOG (1, "Editor.show_tooltip_window: no cached tooltip content");
    return FALSE;
  }

  gtk_widget_show (GTK_WIDGET (self->tooltip_window));
  gtk_window_present (GTK_WINDOW (self->tooltip_window));

  return TRUE;
}
