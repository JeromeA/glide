#include "package.h"
#include "package_common_lisp_user.h"
#include "node.h"
#include <assert.h>

int main(void) {
  Package *package = package_new("CL-USER");
  package_set_description(package, "desc");
  package_add_nickname(package, "USER");
  package_add_use(package, "COMMON-LISP");
  package_add_export(package, "FOO");
  package_add_shadow(package, "BAR");
  package_add_import_from(package, "BAZ", "OTHER");

  assert(g_strcmp0(package->name, "CL-USER") == 0);
  assert(g_strcmp0(package->description, "desc") == 0);
  assert(g_hash_table_contains(package->nicknames, "USER"));
  assert(g_hash_table_contains(package->uses, "COMMON-LISP"));
  assert(g_hash_table_contains(package->exports, "FOO"));
  assert(g_hash_table_contains(package->shadows, "BAR"));
  assert(g_strcmp0(g_hash_table_lookup(package->import_from, "BAZ"), "OTHER") == 0);

  Package *user = package_common_lisp_user_get_instance();
  assert(g_strcmp0(user->name, "COMMON-LISP-USER") == 0);
  assert(g_hash_table_contains(user->nicknames, "CL-USER"));
  assert(g_hash_table_contains(user->uses, "COMMON-LISP"));

  Node *node = g_new0(Node, 1);
  g_atomic_int_set(&node->ref, 1);
  node_set_sd_type(node, SDT_PACKAGE_DEF, "COMMON-LISP-USER");
  assert(node_is(node, SDT_PACKAGE_DEF));
  assert(g_strcmp0(node->package_context, "COMMON-LISP-USER") == 0);

  node_unref(node);
  package_unref(package);
  package_unref(user);
  return 0;
}
