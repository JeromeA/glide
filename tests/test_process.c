#include "process.h"
#include "preferences.h"
#include <glib.h>
#include <glib/gstdio.h>
#include <string.h>

static GString *output;

static void on_out(GString *data, gpointer user_data) {
    g_string_append_len(output, data->str, data->len);
}

static void test_bc(void)
{
    gchar *tmpdir = g_dir_make_tmp("proc-test-XXXXXX", NULL);
    Preferences *prefs = preferences_new(tmpdir);
    gchar *file = g_build_filename(tmpdir, "glide", "preferences.ini", NULL);
    preferences_set_sdk(prefs, "/usr/bin/bc");
    ProcessImpl *p = process_new(prefs);
    g_assert_nonnull(p);
    output = g_string_new(NULL);
    process_set_stdout_cb(p, on_out, NULL);
    process_write(p, "1+2\n", -1);
    g_usleep(100000);
    g_assert_nonnull(strstr(output->str, "3"));
    process_write(p, "quit\n", -1);
    process_free(p);
    g_object_unref(prefs);
    g_remove(file);
    gchar *prefs_dir = g_path_get_dirname(file);
    g_rmdir(prefs_dir);
    g_rmdir(tmpdir);
    g_free(prefs_dir);
    g_free(file);
    g_free(tmpdir);
    g_string_free(output, TRUE);
}

int main(int argc, char *argv[])
{
    g_test_init(&argc, &argv, NULL);
    g_test_add_func("/process/bc", test_bc);
    return g_test_run();
}
