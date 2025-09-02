// This test verifies the basic package API and ensures it integrates
// with Node's package context helpers.
#include "package.h"
#include "node.h"
#include <glib.h>

static void test_package(void) {
  Package *package = package_new("CL-USER");
  package_set_description(package, "desc");
  package_add_nickname(package, "USER");
  package_add_use(package, "COMMON-LISP");
  package_add_export(package, "FOO");
  package_add_shadow(package, "BAR");
  package_add_import_from(package, "BAZ", "OTHER");

  g_assert_cmpstr(package_get_name(package), ==, "CL-USER");
  g_assert_cmpstr(package_get_description(package), ==, "desc");
  g_assert_true(g_hash_table_contains(package_get_nicknames(package), "USER"));
  g_assert_true(g_hash_table_contains(package_get_uses(package), "COMMON-LISP"));
  g_assert_true(g_hash_table_contains(package_get_exports(package), "FOO"));
  g_assert_true(g_hash_table_contains(package_get_shadows(package), "BAR"));
  g_assert_cmpstr(g_hash_table_lookup(package_get_import_from(package), "BAZ"), ==, "OTHER");

  Node *node = g_new0(Node, 1);
  g_atomic_int_set(&node->ref, 1);
  node_set_sd_type(node, SDT_PACKAGE_DEF, "COMMON-LISP-USER");
  g_assert(node_is(node, SDT_PACKAGE_DEF));
  g_assert_cmpstr(node->package_context, ==, "COMMON-LISP-USER");

  node_unref(node);
  package_unref(package);
}

int main(int argc, char *argv[]) {
  g_test_init(&argc, &argv, NULL);
  g_test_add_func("/package/basic", test_package);
  return g_test_run();
}
