#include "package_common_lisp.h"
#include <glib.h>

static void test_singleton(void) {
  Package *p1 = package_common_lisp_get_instance();
  Package *p2 = package_common_lisp_get_instance();
  g_assert_true(p1 == p2);
  package_unref(p1);
  package_unref(p2);
}

static void test_exports(void) {
  Package *p = package_common_lisp_get_instance();
  g_assert_true(g_hash_table_contains(package_get_exports(p), "CAR"));
  g_assert_true(g_hash_table_contains(package_get_exports(p), "*PRINT-LEVEL*"));
  g_assert_true(g_hash_table_contains(package_get_exports(p), "&REST"));
  package_unref(p);
}

static void test_nickname(void) {
  Package *p = package_common_lisp_get_instance();
  g_assert_true(g_hash_table_contains(package_get_nicknames(p), "CL"));
  package_unref(p);
}

int main(int argc, char *argv[]) {
  g_test_init(&argc, &argv, NULL);
  g_test_add_func("/package_common_lisp/singleton", test_singleton);
  g_test_add_func("/package_common_lisp/exports", test_exports);
  g_test_add_func("/package_common_lisp/nickname", test_nickname);
  return g_test_run();
}
