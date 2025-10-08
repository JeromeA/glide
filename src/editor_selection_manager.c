#include "editor_selection_manager.h"

#include "node.h"
#include "util.h"

typedef struct {
  gsize start;
  gsize end;
} SelectionRange;

struct _EditorSelectionManager
{
  GObject parent_instance;
  GArray *selection_stack;
};

G_DEFINE_TYPE(EditorSelectionManager, editor_selection_manager, G_TYPE_OBJECT)

EditorSelectionManager *
editor_selection_manager_new(void)
{
  return g_object_new(EDITOR_TYPE_SELECTION_MANAGER, NULL);
}

static void
editor_selection_manager_select_range(GtkTextBuffer *buffer, gsize start, gsize end)
{
  GtkTextIter it_start;
  GtkTextIter it_end;
  gtk_text_buffer_get_iter_at_offset(buffer, &it_start, start);
  gtk_text_buffer_get_iter_at_offset(buffer, &it_end, end);
  gtk_text_buffer_select_range(buffer, &it_start, &it_end);
}

static void
editor_selection_manager_dispose(GObject *object)
{
  EditorSelectionManager *self = GLIDE_EDITOR_SELECTION_MANAGER(object);

  if (self->selection_stack) {
    g_array_free(self->selection_stack, TRUE);
    self->selection_stack = NULL;
  }

  G_OBJECT_CLASS(editor_selection_manager_parent_class)->dispose(object);
}

static void
editor_selection_manager_class_init(EditorSelectionManagerClass *klass)
{
  GObjectClass *object_class = G_OBJECT_CLASS(klass);
  object_class->dispose = editor_selection_manager_dispose;
}

static void
editor_selection_manager_init(EditorSelectionManager *self)
{
  self->selection_stack = g_array_new(FALSE, FALSE, sizeof(SelectionRange));
}

gboolean
editor_selection_manager_find_parent_range(EditorSelectionManager *self, GtkTextBuffer *buffer,
    Document *document, gsize start, gsize end, gsize *new_start, gsize *new_end)
{
  g_return_val_if_fail(GLIDE_IS_EDITOR_SELECTION_MANAGER(self), FALSE);
  g_return_val_if_fail(GTK_IS_TEXT_BUFFER(buffer), FALSE);
  g_return_val_if_fail(document != NULL, FALSE);
  g_return_val_if_fail(new_start != NULL, FALSE);
  g_return_val_if_fail(new_end != NULL, FALSE);

  GtkTextIter end_iter;
  gtk_text_buffer_get_end_iter(buffer, &end_iter);
  gsize len = gtk_text_iter_get_offset(&end_iter);

  LOG(1, "find_parent_range start=%zu end=%zu", start, end);

  const Node *ast = document_get_ast(document);
  if (!ast)
    return FALSE;

  const Node *node = node_find_containing_range(ast, start, end);
  if (!node) {
    LOG(1, "no node found");
    return FALSE;
  }

  gsize node_start = node_get_start_offset(node);
  gsize node_end = node_get_end_offset(node);
  const Node *parent = node->parent;

  while (node_start == start && node_end == end && parent) {
    LOG(1, "parent has same range [%zu,%zu), climbing", node_start, node_end);
    node = parent;
    node_start = node_get_start_offset(node);
    node_end = node_get_end_offset(node);
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

void
editor_selection_manager_extend(EditorSelectionManager *self, GtkTextBuffer *buffer, Document *document)
{
  g_return_if_fail(GLIDE_IS_EDITOR_SELECTION_MANAGER(self));
  g_return_if_fail(GTK_IS_TEXT_BUFFER(buffer));
  if (!document)
    return;

  GtkTextIter it_start;
  GtkTextIter it_end;
  if (!gtk_text_buffer_get_selection_bounds(buffer, &it_start, &it_end)) {
    GtkTextMark *mark = gtk_text_buffer_get_insert(buffer);
    gtk_text_buffer_get_iter_at_mark(buffer, &it_start, mark);
    it_end = it_start;
  }

  gsize start = gtk_text_iter_get_offset(&it_start);
  gsize end = gtk_text_iter_get_offset(&it_end);

  LOG(1, "extend_selection start=%zu end=%zu stack_len=%u", start, end,
      self->selection_stack ? self->selection_stack->len : 0);

  if (!self->selection_stack)
    self->selection_stack = g_array_new(FALSE, FALSE, sizeof(SelectionRange));

  if (self->selection_stack->len == 0) {
    SelectionRange original = { start, end };
    g_array_append_val(self->selection_stack, original);
  }

  gsize new_start = start;
  gsize new_end = end;
  if (!editor_selection_manager_find_parent_range(self, buffer, document, start, end, &new_start, &new_end)) {
    LOG(1, "no parent range found, resetting");
    if (self->selection_stack->len > 0) {
      SelectionRange orig = g_array_index(self->selection_stack, SelectionRange, 0);
      editor_selection_manager_select_range(buffer, orig.start, orig.end);
    }
    g_array_set_size(self->selection_stack, 0);
    return;
  }

  SelectionRange new_range = { new_start, new_end };
  g_array_append_val(self->selection_stack, new_range);
  editor_selection_manager_select_range(buffer, new_start, new_end);
  LOG(1, "selection extended to [%zu,%zu)", new_start, new_end);
}

void
editor_selection_manager_shrink(EditorSelectionManager *self, GtkTextBuffer *buffer)
{
  g_return_if_fail(GLIDE_IS_EDITOR_SELECTION_MANAGER(self));
  g_return_if_fail(GTK_IS_TEXT_BUFFER(buffer));
  if (!self->selection_stack || self->selection_stack->len == 0)
    return;

  if (self->selection_stack->len <= 1) {
    SelectionRange orig = g_array_index(self->selection_stack, SelectionRange, 0);
    editor_selection_manager_select_range(buffer, orig.start, orig.end);
    g_array_set_size(self->selection_stack, 0);
    return;
  }

  g_array_remove_index(self->selection_stack, self->selection_stack->len - 1);
  SelectionRange prev = g_array_index(self->selection_stack, SelectionRange,
      self->selection_stack->len - 1);
  editor_selection_manager_select_range(buffer, prev.start, prev.end);
}
