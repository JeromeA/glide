
#include "app.h"
#include "preferences.h"
#include "process.h"
#include "repl_process.h"
#include "repl_session.h"
#include "status_service.h"
#include "util.h"
#include <gtk/gtk.h>
#include <gtksourceview/gtksource.h>

int
main (int argc, char *argv[])
{
  LOG(1, "Main.main");
  Preferences *prefs = preferences_new (g_get_user_config_dir ());
  const gchar *sdk_path = preferences_get_sdk(prefs);
  const gchar *proc_argv[] = {
    sdk_path, "--noinform",
    "--eval", "(require :glide)",
    "--eval", "(glide:start-server)",
    NULL
  };
  Process *proc = process_new_from_argv(proc_argv);
  ReplProcess *repl_proc = repl_process_new(proc);
  StatusService *status_service = status_service_new();
  ReplSession *repl = repl_session_new(repl_proc, status_service);
  App *app     = app_new (prefs, repl, status_service);

  int status = g_application_run (G_APPLICATION (app), argc, argv);
  g_object_unref(app);
  repl_session_unref(repl);
  repl_process_unref(repl_proc);
  process_unref(proc);
  status_service_free(status_service);
  preferences_unref(prefs);
  exit(status);
}

