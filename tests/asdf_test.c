#include "asdf.h"
#include <glib.h>
#include <glib/gstdio.h>

static void test_parse(void)
{
  gchar *tmpdir = g_dir_make_tmp("asdf-test-XXXXXX", NULL);
  gchar *file = g_build_filename(tmpdir, "foo.asd", NULL);
  const gchar *contents = "(defsystem \"foo\"\n  :serial t\n  :components ((file \"a\") (file \"b\"))\n  :depends-on (\"dep1\" \"dep2\"))";
  g_file_set_contents(file, contents, -1, NULL);

  Asdf *asdf = asdf_new_from_file(file);
  g_assert_true(asdf_get_serial(asdf));
  g_assert_cmpuint(asdf_get_component_count(asdf), ==, 2);
  g_assert_cmpstr(asdf_get_component(asdf, 0), ==, "a");
  g_assert_cmpstr(asdf_get_component(asdf, 1), ==, "b");
  g_assert_cmpuint(asdf_get_dependency_count(asdf), ==, 2);
  g_assert_cmpstr(asdf_get_dependency(asdf, 0), ==, "dep1");
  g_assert_cmpstr(asdf_get_dependency(asdf, 1), ==, "dep2");

  gchar *str = asdf_to_string(asdf);
  g_assert_null(strstr(str, "pathname"));
  g_free(str);

  g_object_unref(asdf);
  g_remove(file);
  g_rmdir(tmpdir);
  g_free(file);
  g_free(tmpdir);
}

static void test_save(void)
{
  gchar *tmpdir = g_dir_make_tmp("asdf-test-XXXXXX", NULL);
  gchar *out = g_build_filename(tmpdir, "out.asd", NULL);

  Asdf *asdf = asdf_new();
  asdf_set_serial(asdf, TRUE);
  asdf_add_component(asdf, "a");
  asdf_add_component(asdf, "b");
  asdf_add_dependency(asdf, "dep1");
  asdf_add_dependency(asdf, "dep2");

  g_assert_true(asdf_save(asdf, out));
  g_assert_true(g_file_test(out, G_FILE_TEST_EXISTS));

  gchar *contents = NULL;
  g_file_get_contents(out, &contents, NULL, NULL);
  g_assert_nonnull(contents);
  g_assert_nonnull(strstr(contents, "(defsystem \"system\""));
  g_free(contents);

  g_object_unref(asdf);
  g_remove(out);
  g_rmdir(tmpdir);
  g_free(out);
  g_free(tmpdir);
}

static void test_rename(void)
{
  Asdf *asdf = asdf_new();
  asdf_add_component(asdf, "old");
  asdf_rename_component(asdf, "old", "new");
  g_assert_cmpstr(asdf_get_component(asdf, 0), ==, "new");
  g_object_unref(asdf);
}

static void test_remove(void)
{
  Asdf *asdf = asdf_new();
  asdf_add_component(asdf, "a");
  asdf_add_component(asdf, "b");
  asdf_remove_component(asdf, "a");
  g_assert_cmpuint(asdf_get_component_count(asdf), ==, 1);
  g_assert_cmpstr(asdf_get_component(asdf, 0), ==, "b");
  g_object_unref(asdf);
}

int main(int argc, char *argv[])
{
  g_test_init(&argc, &argv, NULL);
  g_test_add_func("/asdf/parse", test_parse);
  g_test_add_func("/asdf/save", test_save);
  g_test_add_func("/asdf/rename", test_rename);
  g_test_add_func("/asdf/remove", test_remove);
  return g_test_run();
}
