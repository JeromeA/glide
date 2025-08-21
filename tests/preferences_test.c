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

int
main(int argc, char *argv[])
{
    g_test_init(&argc, &argv, NULL);

    g_test_add_func("/preferences/defaults", test_defaults);
    g_test_add_func("/preferences/set_sdk", test_set_sdk);

    return g_test_run();
}

