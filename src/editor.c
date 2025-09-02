#include "editor.h"
#include "gtk_text_provider.h"
#include "project.h"
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
  ProjectFile *file;
  GArray *selection_stack;
};

G_DEFINE_TYPE (Editor, editor, GTK_TYPE_SCROLLED_WINDOW)

// Forward declaration for the callback
static void on_buffer_changed (GtkTextBuffer *buffer, gpointer user_data);

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

  self->project = NULL;
  self->file = NULL;
  self->selection_stack = g_array_new (FALSE, FALSE, sizeof (SelectionRange));
}

// Callback for when the GtkTextBuffer changes
static void
on_buffer_changed (GtkTextBuffer * /*buffer*/, gpointer user_data)
{
  Editor *self = GLIDE_EDITOR (user_data);
  if (self && self->project && self->file)
    project_file_changed(self->project, self->file);
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

  g_clear_object (&self->buffer);
  self->view = NULL;
  if (self->selection_stack) {
    g_array_free (self->selection_stack, TRUE);
    self->selection_stack = NULL;
  }

  G_OBJECT_CLASS (editor_parent_class)->dispose (object);
}

static void
editor_class_init (EditorClass *klass)
{
  GObjectClass *object_class = G_OBJECT_CLASS (klass);
  object_class->dispose = editor_dispose;
}

GtkWidget *
editor_new_for_file (Project *project, ProjectFile *file)
{
  g_return_val_if_fail(project != NULL, NULL);
  g_return_val_if_fail(file != NULL, NULL);

  Editor *self = g_object_new (EDITOR_TYPE, NULL);
  self->project = project_ref(project);
  self->file = file;

  TextProvider *existing = project_file_get_provider(self->file);
  if (existing) {
    gsize len = text_provider_get_length(existing);
    gchar *text = text_provider_get_text(existing, 0, len);
    gtk_source_buffer_begin_not_undoable_action(self->buffer);
    gtk_text_buffer_set_text(GTK_TEXT_BUFFER(self->buffer), text, -1);
    gtk_source_buffer_end_not_undoable_action(self->buffer);
    g_free(text);
  }

  TextProvider *provider = gtk_text_provider_new (GTK_TEXT_BUFFER (self->buffer));
  project_file_set_provider (self->file, provider, GTK_TEXT_BUFFER (self->buffer));
  text_provider_unref (provider);
  project_file_changed (self->project, self->file);
  gtk_text_buffer_set_modified (GTK_TEXT_BUFFER (self->buffer), FALSE);
  g_signal_connect (self->buffer, "changed", G_CALLBACK (on_buffer_changed), self);
  return GTK_WIDGET (self);
}


GtkSourceBuffer *
editor_get_buffer (Editor *self)
{
  g_return_val_if_fail (GLIDE_IS_EDITOR (self), NULL);
  return self->buffer;
}

ProjectFile *
editor_get_file(Editor *self)
{
  g_return_val_if_fail(GLIDE_IS_EDITOR(self), NULL);
  return self->file;
}

GtkWidget *
editor_get_view (Editor *self)
{
  g_return_val_if_fail (GLIDE_IS_EDITOR (self), NULL);
  return GTK_WIDGET (self->view);
}

static gboolean find_parent_range (GtkTextBuffer *buffer, ProjectFile *file,
    gsize start, gsize end, gsize *new_start, gsize *new_end);

gboolean
editor_get_toplevel_range (Editor *self, gsize offset,
    gsize *start, gsize *end)
{
  g_return_val_if_fail (GLIDE_IS_EDITOR (self), FALSE);
  g_return_val_if_fail (start != NULL, FALSE);
  g_return_val_if_fail (end != NULL, FALSE);
  GtkTextBuffer *buffer = GTK_TEXT_BUFFER(self->buffer);
  if (!self->file)
    return FALSE;

  GtkTextIter end_iter;
  gtk_text_buffer_get_end_iter(buffer, &end_iter);
  gsize len = gtk_text_iter_get_offset(&end_iter);

  gsize cur_start = offset;
  gsize cur_end = offset;
  gsize new_start;
  gsize new_end;

  while (find_parent_range(buffer, self->file, cur_start, cur_end,
      &new_start, &new_end)) {
    if (new_start == 0 && new_end == len)
      break;
    cur_start = new_start;
    cur_end = new_end;
  }

  if (cur_start == offset && cur_end == offset)
    return FALSE;

  *start = cur_start;
  *end = cur_end;
  return TRUE;
}

static const Node *
find_node_containing_range (const Node *node, gsize start, gsize end, gsize len)
{
  if (!node)
    return NULL;
  gsize node_start = node->start_token ? node->start_token->start_offset : 0;
  gsize node_end = node->end_token ? node->end_token->end_offset : len;
  if (start < node_start || end > node_end)
    return NULL;
  for (guint i = 0; node->children && i < node->children->len; i++) {
    const Node *child = g_array_index (node->children, Node*, i);
    const Node *found = find_node_containing_range (child, start, end, len);
    if (found)
      return found;
  }
  return node;
}

static gboolean
find_parent_range (GtkTextBuffer *buffer, ProjectFile *file,
    gsize start, gsize end, gsize *new_start, gsize *new_end)
{
  GtkTextIter end_iter;
  gtk_text_buffer_get_end_iter (buffer, &end_iter);
  gsize len = gtk_text_iter_get_offset (&end_iter);

  g_debug ("find_parent_range start=%zu end=%zu", start, end);

  LispParser *parser = project_file_get_parser (file);
  const Node *ast = lisp_parser_get_ast (parser);
  const Node *node = find_node_containing_range (ast, start, end, len);
  if (!node) {
    g_debug ("no node found");
    return FALSE;
  }

  gsize node_start = node->start_token ? node->start_token->start_offset : 0;
  gsize node_end = node->end_token ? node->end_token->end_offset : len;
  const Node *parent = node->parent;

  while (node_start == start && node_end == end && parent) {
    g_debug ("parent has same range [%zu,%zu), climbing", node_start, node_end);
    node = parent;
    node_start = node->start_token ? node->start_token->start_offset : 0;
    node_end = node->end_token ? node->end_token->end_offset : len;
    parent = node->parent;
  }

  if (node_start == start && node_end == end) {
    if (start == 0 && end == len) {
      g_debug ("at buffer range, cannot extend");
      return FALSE;
    }
    *new_start = 0;
    *new_end = len;
    g_debug ("extending to buffer [%zu,%zu)", *new_start, *new_end);
    return TRUE;
  }

  *new_start = node_start;
  *new_end = node_end;
  g_debug ("new range [%zu,%zu)", *new_start, *new_end);
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

  g_debug ("extend_selection start=%zu end=%zu stack_len=%u", start, end,
      self->selection_stack->len);

  if (self->selection_stack->len == 0) {
    SelectionRange original = { start, end };
    g_array_append_val (self->selection_stack, original);
  }

  gsize new_start = start;
  gsize new_end = end;
  if (!find_parent_range (buffer, self->file, start, end, &new_start, &new_end)) {
    g_debug ("no parent range found, resetting");
    SelectionRange orig = g_array_index (self->selection_stack, SelectionRange, 0);
    select_range (buffer, orig.start, orig.end);
    g_array_set_size (self->selection_stack, 0);
    return;
  }

  SelectionRange new_range = { new_start, new_end };
  g_array_append_val (self->selection_stack, new_range);
  select_range (buffer, new_start, new_end);
  g_debug ("selection extended to [%zu,%zu)", new_start, new_end);
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
