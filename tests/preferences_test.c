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
    g_assert_cmpint(preferences_get_window_width(prefs), ==, 800);
    g_assert_cmpint(preferences_get_window_height(prefs), ==, 600);

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

static void
test_set_window_size(void)
{
  gchar *tmpdir = g_dir_make_tmp("prefs-test-XXXXXX", NULL);
  Preferences *prefs = preferences_new(tmpdir);
  gchar *file = g_build_filename(tmpdir, "glide", "preferences.ini", NULL);

  preferences_set_window_width(prefs, 1024);
  preferences_set_window_height(prefs, 768);
  g_assert_cmpint(preferences_get_window_width(prefs), ==, 1024);
  g_assert_cmpint(preferences_get_window_height(prefs), ==, 768);

  gchar *contents = NULL;
  g_file_get_contents(file, &contents, NULL, NULL);
  g_assert_nonnull(contents);
  g_assert_nonnull(strstr(contents, "1024"));
  g_assert_nonnull(strstr(contents, "768"));
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
test_recent_projects(void)
{
  gchar *tmpdir = g_dir_make_tmp("prefs-test-XXXXXX", NULL);
  Preferences *prefs = preferences_new(tmpdir);

  preferences_add_recent_project(prefs, "/tmp/a");
  preferences_add_recent_project(prefs, "/tmp/b");
  preferences_add_recent_project(prefs, "/tmp/c");
  preferences_add_recent_project(prefs, "/tmp/d");
  preferences_add_recent_project(prefs, "/tmp/e");
  preferences_add_recent_project(prefs, "/tmp/f");

  const GList *list = preferences_get_recent_projects(prefs);
  g_assert_cmpuint(g_list_length((GList *)list), ==, 5);
  g_assert_cmpstr(list->data, ==, "/tmp/f");
  g_assert_cmpstr(g_list_nth_data((GList *)list, 4), ==, "/tmp/b");

  preferences_add_recent_project(prefs, "/tmp/d");
  list = preferences_get_recent_projects(prefs);
  g_assert_cmpstr(list->data, ==, "/tmp/d");
  g_assert_cmpstr(g_list_nth_data((GList *)list, 1), ==, "/tmp/f");

  preferences_unref(prefs);
  prefs = preferences_new(tmpdir);
  list = preferences_get_recent_projects(prefs);
  g_assert_cmpstr(list->data, ==, "/tmp/d");
  preferences_unref(prefs);

  gchar *file = g_build_filename(tmpdir, "glide", "preferences.ini", NULL);
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
    g_test_add_func("/preferences/set_window_size", test_set_window_size);
    g_test_add_func("/preferences/recent_projects", test_recent_projects);

    return g_test_run();
}

