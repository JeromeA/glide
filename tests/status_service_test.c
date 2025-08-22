#include "status_service.h"
#include <glib.h>

static gchar *last = NULL;

static void
on_changed(const gchar *text, gpointer data)
{
  g_free(last);
  last = g_strdup(text);
}

static void
test_publish_update_unpublish(void)
{
  StatusService *service = status_service_new();
  status_service_set_callback(service, on_changed, NULL);
  g_assert_cmpstr(last, ==, "Ready");
  guint id1 = status_service_publish(service, "One");
  g_assert_cmpstr(last, ==, "One");
  guint id2 = status_service_publish(service, "Two");
  g_assert_cmpstr(last, ==, "One - Two");
  status_service_update(service, id1, "Uno");
  g_assert_cmpstr(last, ==, "Uno - Two");
  status_service_unpublish(service, id2);
  g_assert_cmpstr(last, ==, "Uno");
  status_service_unpublish(service, id1);
  g_assert_cmpstr(last, ==, "Ready");
  status_service_free(service);
  g_free(last);
  last = NULL;
}

int
main(int argc, char *argv[])
{
  g_test_init(&argc, &argv, NULL);
  g_test_add_func("/status_service/publish", test_publish_update_unpublish);
  return g_test_run();
}
