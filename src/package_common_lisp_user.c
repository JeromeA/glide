#include "package_common_lisp_user.h"

static gpointer package_common_lisp_user_create(gpointer /*unused*/) {
  Package *package = package_new("COMMON-LISP-USER");
  package_add_nickname(package, "CL-USER");
  package_add_use(package, "COMMON-LISP");
  return package;
}

Package *package_common_lisp_user_get_instance(void) {
  static GOnce once = G_ONCE_INIT;
  return package_ref(g_once(&once,
        (GThreadFunc)package_common_lisp_user_create, NULL));
}

