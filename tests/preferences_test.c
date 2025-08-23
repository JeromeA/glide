#include "preferences.h"
#include <glib.h>
#include <glib/gstdio.h>
#include <string.h>

static void
test_defaults(void)
{
    gchar *tmpdir = g_dir_make_tmp("prefs-test-XXXXXX", NULL);

    Preferences *prefs = preferences_new(tmpdir);
    gchar *file = g_build_filename(tmpdir, "glide", "preferences.ini", NULL);

    g_assert_null(preferences_get_sdk(prefs));
    g_assert_cmpuint(preferences_get_swank_port(prefs), ==, 4005);
    g_assert_cmpstr(preferences_get_project_dir(prefs), ==, "~/lisp");
    g_assert_cmpint(preferences_get_asdf_view_width(prefs), ==, 200);

    preferences_unref(prefs);

    g_remove(file);
    gchar *prefs_dir = g_path_get_dirname(file);
    g_rmdir(prefs_dir);
    g_rmdir(tmpdir);
    g_free(prefs_dir);
    g_free(file);
    g_free(tmpdir);
}

static void
test_set_sdk(void)
{
    gchar *tmpdir = g_dir_make_tmp("prefs-test-XXXXXX", NULL);
    Preferences *prefs = preferences_new(tmpdir);
    gchar *file = g_build_filename(tmpdir, "glide", "preferences.ini", NULL);

    preferences_set_sdk(prefs, "my_sdk");
    g_assert_cmpstr(preferences_get_sdk(prefs), ==, "my_sdk");

    gchar *contents = NULL;
    g_file_get_contents(file, &contents, NULL, NULL);
    g_assert_nonnull(contents);
    g_assert_nonnull(strstr(contents, "my_sdk"));
    g_free(contents);

    preferences_unref(prefs);

    g_remove(file);
    gchar *prefs_dir = g_path_get_dirname(file);
    g_rmdir(prefs_dir);
    g_rmdir(tmpdir);
    g_free(prefs_dir);
    g_free(file);
    g_free(tmpdir);
}

static void
test_set_project_dir(void)
{
  gchar *tmpdir = g_dir_make_tmp("prefs-test-XXXXXX", NULL);
  Preferences *prefs = preferences_new(tmpdir);
  gchar *file = g_build_filename(tmpdir, "glide", "preferences.ini", NULL);

  preferences_set_project_dir(prefs, "/tmp/foo");
  g_assert_cmpstr(preferences_get_project_dir(prefs), ==, "/tmp/foo");

  gchar *contents = NULL;
  g_file_get_contents(file, &contents, NULL, NULL);
  g_assert_nonnull(contents);
  g_assert_nonnull(strstr(contents, "/tmp/foo"));
  g_free(contents);

  preferences_unref(prefs);

  g_remove(file);
  gchar *prefs_dir = g_path_get_dirname(file);
  g_rmdir(prefs_dir);
  g_rmdir(tmpdir);
  g_free(prefs_dir);
  g_free(file);
  g_free(tmpdir);
}

static void
test_set_asdf_view_width(void)
{
  gchar *tmpdir = g_dir_make_tmp("prefs-test-XXXXXX", NULL);
  Preferences *prefs = preferences_new(tmpdir);
  gchar *file = g_build_filename(tmpdir, "glide", "preferences.ini", NULL);

  preferences_set_asdf_view_width(prefs, 300);
  g_assert_cmpint(preferences_get_asdf_view_width(prefs), ==, 300);

  gchar *contents = NULL;
  g_file_get_contents(file, &contents, NULL, NULL);
  g_assert_nonnull(contents);
  g_assert_nonnull(strstr(contents, "300"));
  g_free(contents);

  preferences_unref(prefs);

  g_remove(file);
  gchar *prefs_dir = g_path_get_dirname(file);
  g_rmdir(prefs_dir);
  g_rmdir(tmpdir);
  g_free(prefs_dir);
  g_free(file);
  g_free(tmpdir);
}

int
main(int argc, char *argv[])
{
    g_test_init(&argc, &argv, NULL);

    g_test_add_func("/preferences/defaults", test_defaults);
    g_test_add_func("/preferences/set_sdk", test_set_sdk);
    g_test_add_func("/preferences/set_project_dir", test_set_project_dir);
    g_test_add_func("/preferences/set_asdf_view_width", test_set_asdf_view_width);

    return g_test_run();
}

