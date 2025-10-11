#include "editor_selection_manager.h"
#include <gtk/gtk.h>
#include <gtksourceview/gtksource.h>

static gboolean have_display;

static void
assert_selection(GtkTextBuffer *buffer, gsize expected_start, gsize expected_end)
{
  GtkTextIter start;
  GtkTextIter end;
  g_assert_true(gtk_text_buffer_get_selection_bounds(buffer, &start, &end));
  g_assert_cmpuint(gtk_text_iter_get_offset(&start), ==, expected_start);
  g_assert_cmpuint(gtk_text_iter_get_offset(&end), ==, expected_end);
}

static void
assert_cursor(GtkTextBuffer *buffer, gsize expected_offset)
{
  GtkTextIter iter;
  gtk_text_buffer_get_iter_at_mark(buffer, &iter, gtk_text_buffer_get_insert(buffer));
  g_assert_cmpuint(gtk_text_iter_get_offset(&iter), ==, expected_offset);
}

static void
test_extend_and_shrink_nested_ranges(void)
{
  if (!have_display) {
    g_test_skip("no display");
    return;
  }

  const gchar *text = "(foo (bar baz))";
  GtkTextBuffer *buffer = GTK_TEXT_BUFFER(gtk_source_buffer_new(NULL));
  gtk_text_buffer_set_text(buffer, text, -1);
  Document *document = document_new(NULL, DOCUMENT_LIVE);
  document_set_content(document, g_string_new(text));
  EditorSelectionManager *manager = editor_selection_manager_new();

  GtkTextIter iter;
  gtk_text_buffer_get_iter_at_offset(buffer, &iter, 7);
  gtk_text_buffer_place_cursor(buffer, &iter);

  editor_selection_manager_extend(manager, buffer, document);
  assert_selection(buffer, 6, 9);

  editor_selection_manager_extend(manager, buffer, document);
  assert_selection(buffer, 5, 14);

  editor_selection_manager_extend(manager, buffer, document);
  assert_selection(buffer, 0, 15);

  editor_selection_manager_extend(manager, buffer, document);
  g_assert_false(gtk_text_buffer_get_selection_bounds(buffer, NULL, NULL));
  assert_cursor(buffer, 7);

  gtk_text_buffer_place_cursor(buffer, &iter);
  editor_selection_manager_extend(manager, buffer, document);
  editor_selection_manager_extend(manager, buffer, document);
  assert_selection(buffer, 5, 14);

  editor_selection_manager_shrink(manager, buffer);
  assert_selection(buffer, 6, 9);

  editor_selection_manager_shrink(manager, buffer);
  g_assert_false(gtk_text_buffer_get_selection_bounds(buffer, NULL, NULL));
  assert_cursor(buffer, 7);

  editor_selection_manager_shrink(manager, buffer);
  g_assert_false(gtk_text_buffer_get_selection_bounds(buffer, NULL, NULL));
  assert_cursor(buffer, 7);

  g_object_unref(buffer);
  document_free(document);
  g_object_unref(manager);
}

int
main(int argc, char *argv[])
{
  g_test_init(&argc, &argv, NULL);
  have_display = gtk_init_check(&argc, &argv);
  GMainContext *ctx = g_main_context_default();
  g_main_context_push_thread_default(ctx);
  g_test_add_func("/editor_selection_manager/extend_and_shrink_nested_ranges",
      test_extend_and_shrink_nested_ranges);
  int ret = g_test_run();
  g_main_context_pop_thread_default(ctx);
  return ret;
}
