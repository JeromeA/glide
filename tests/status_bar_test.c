#include "status_bar.h"
#include <gtk/gtk.h>
#include <glib.h>

static const gchar *
get_text(StatusBar *bar)
{
  GList *children = gtk_container_get_children(GTK_CONTAINER(bar));
  GtkWidget *label = GTK_WIDGET(children->data);
  const gchar *text = gtk_label_get_text(GTK_LABEL(label));
  g_list_free(children);
  return text;
}

static void
test_defaults(void)
{
  StatusBar *bar = status_bar_new();
  g_assert_cmpstr(get_text(bar), ==, "Ready");
  g_object_unref(bar);
}

static void
test_publish_update_unpublish(void)
{
  StatusBar *bar = status_bar_new();
  guint id1 = status_bar_publish(bar, "One");
  g_assert_cmpstr(get_text(bar), ==, "One");
  guint id2 = status_bar_publish(bar, "Two");
  g_assert_cmpstr(get_text(bar), ==, "One - Two");
  status_bar_update(bar, id1, "Uno");
  g_assert_cmpstr(get_text(bar), ==, "Uno - Two");
  status_bar_unpublish(bar, id2);
  g_assert_cmpstr(get_text(bar), ==, "Uno");
  status_bar_unpublish(bar, id1);
  g_assert_cmpstr(get_text(bar), ==, "Ready");
  g_object_unref(bar);
}

int
main(int argc, char *argv[])
{
  g_test_init(&argc, &argv, NULL);
  if (!gtk_init_check(&argc, &argv))
    return g_test_run();

  g_test_add_func("/status_bar/default", test_defaults);
  g_test_add_func("/status_bar/publish", test_publish_update_unpublish);

  return g_test_run();
}
