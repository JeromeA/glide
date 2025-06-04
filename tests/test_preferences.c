#include "preferences.h"
#include <glib.h>
#include <string.h>

static void
test_defaults(void)
{
    if (g_test_subprocess()) {
        Preferences *prefs = preferences_get_instance();
        g_assert_null(preferences_get_sdk(prefs));
        g_assert_cmpuint(preferences_get_swank_port(prefs), ==, 4005);
        return;
    }

    gchar *tmpdir = g_dir_make_tmp("prefs-test-XXXXXX", NULL);
    gchar *env = g_strdup_printf("XDG_CONFIG_HOME=%s", tmpdir);
    const char *const envp[] = { env, NULL };

    g_test_trap_subprocess_with_envp(NULL, envp, 0, 0);
    g_test_trap_assert_passed();

    g_free(env);
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
    if (g_test_subprocess()) {
        Preferences *prefs = preferences_get_instance();
        int count = 0;
        g_signal_connect(prefs, "sdk-changed", G_CALLBACK(on_sdk_changed), &count);
        preferences_set_sdk(prefs, "my_sdk");
        g_assert_cmpstr(preferences_get_sdk(prefs), ==, "my_sdk");
        g_assert_cmpint(count, ==, 1);

        const char *config_dir = g_getenv("XDG_CONFIG_HOME");
        gchar *filename = g_build_filename(config_dir, "glide", "preferences.ini", NULL);
        gchar *contents = NULL;
        g_file_get_contents(filename, &contents, NULL, NULL);
        g_assert_nonnull(contents);
        g_assert_nonnull(strstr(contents, "my_sdk"));
        g_free(contents);
        g_free(filename);
        return;
    }

    gchar *tmpdir = g_dir_make_tmp("prefs-test-XXXXXX", NULL);
    gchar *env = g_strdup_printf("XDG_CONFIG_HOME=%s", tmpdir);
    const char *const envp[] = { env, NULL };

    g_test_trap_subprocess_with_envp(NULL, envp, 0, 0);
    g_test_trap_assert_passed();

    g_free(env);
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

