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

    g_object_unref(prefs);

    g_remove(file);
    gchar *prefs_dir = g_path_get_dirname(file);
    g_rmdir(prefs_dir);
    g_rmdir(tmpdir);
    g_free(prefs_dir);
    g_free(file);
    g_free(tmpdir);
}

static void
on_sdk_changed(Preferences *prefs, const gchar *sdk, gpointer user_data)
{
    int *count = user_data;
    (*count)++;
}

static void
test_set_sdk(void)
{
    gchar *tmpdir = g_dir_make_tmp("prefs-test-XXXXXX", NULL);

    Preferences *prefs = preferences_new(tmpdir);
    gchar *file = g_build_filename(tmpdir, "glide", "preferences.ini", NULL);

    int count = 0;
    g_signal_connect(prefs, "sdk-changed", G_CALLBACK(on_sdk_changed), &count);

    preferences_set_sdk(prefs, "my_sdk");
    g_assert_cmpstr(preferences_get_sdk(prefs), ==, "my_sdk");
    g_assert_cmpint(count, ==, 1);

    gchar *contents = NULL;
    g_file_get_contents(file, &contents, NULL, NULL);
    g_assert_nonnull(contents);
    g_assert_nonnull(strstr(contents, "my_sdk"));
    g_free(contents);

    g_object_unref(prefs);

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

