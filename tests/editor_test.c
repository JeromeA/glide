#include "editor.h"
#include <gtk/gtk.h>

static gboolean have_display;

static void test_undo_pristine(void)
{
  if (!have_display) {
    g_test_skip("no display");
    return;
  }
  Project *project = project_new(NULL);
  Document *document = project_add_document(project, g_string_new("(a b)"), NULL, DOCUMENT_LIVE);

  GtkWidget *widget = editor_new_for_document(project, document);
  Editor *editor = GLIDE_EDITOR(widget);
  GtkSourceBuffer *buffer = editor_get_buffer(editor);

  g_assert_false(gtk_source_buffer_can_undo(buffer));

  g_object_unref(widget);
  project_unref(project);
}

static void test_toplevel_range_eof_without_newline(void)
{
  if (!have_display) {
    g_test_skip("no display");
    return;
  }
  Project *project = project_new(NULL);
  Document *document = project_add_document(project, g_string_new("(+ 1 2)"), NULL,
      DOCUMENT_LIVE);

  GtkWidget *widget = editor_new_for_document(project, document);
  Editor *editor = GLIDE_EDITOR(widget);

  gsize start;
  gsize end;
  g_assert_true(editor_get_toplevel_range(editor, 7, &start, &end));
  g_assert_cmpint(start, ==, 0);
  g_assert_cmpint(end, ==, 7);

  g_object_unref(widget);
  project_unref(project);
}

int main(int argc, char *argv[])
{
  g_test_init(&argc, &argv, NULL);
  have_display = gtk_init_check(&argc, &argv);
  GMainContext *ctx = g_main_context_default();
  g_main_context_push_thread_default(ctx);
  g_test_add_func("/editor/undo_pristine", test_undo_pristine);
  g_test_add_func("/editor/toplevel_range_eof_without_newline",
      test_toplevel_range_eof_without_newline);
  int ret = g_test_run();
  g_main_context_pop_thread_default(ctx);
  return ret;
}

