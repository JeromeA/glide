#include "editor.h"
#include "string_text_provider.h"
#include <gtk/gtk.h>

static gboolean have_display;

static void test_undo_pristine(void)
{
  if (!have_display) {
    g_test_skip("no display");
    return;
  }
  Project *project = project_new(NULL);
  TextProvider *provider = string_text_provider_new("(a b)");
  ProjectFile *file = project_add_file(project, provider, NULL, NULL, PROJECT_FILE_LIVE);
  text_provider_unref(provider);

  GtkWidget *widget = editor_new_for_file(project, file);
  Editor *editor = GLIDE_EDITOR(widget);
  GtkSourceBuffer *buffer = editor_get_buffer(editor);

  g_assert_false(gtk_source_buffer_can_undo(buffer));

  g_object_unref(widget);
  project_unref(project);
}

int main(int argc, char *argv[])
{
  g_test_init(&argc, &argv, NULL);
  have_display = gtk_init_check(&argc, &argv);
  g_test_add_func("/editor/undo_pristine", test_undo_pristine);
  return g_test_run();
}

