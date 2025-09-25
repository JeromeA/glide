
#ifdef INLINE
#define STATIC static
#include "analyse.c"
#include "analyse_defpackage.c"
#include "analyse_defun.c"
#include "actions.c"
#include "menu_bar.c"
#include "app.c"
#include "asdf.c"
#include "project_view.c"
#include "evaluate.c"
#include "function.c"
#include "file_open.c"
#include "file_new.c"
#include "file_add.c"
#include "file_rename.c"
#include "file_delete.c"
#include "project_new_wizard.c"
#include "file_save.c"
#include "interactions_view.c"
#include "lisp_lexer.c"
#include "lisp_parser.c"
#include "lisp_parser_view.c"
#include "editor_container.c"
#include "editor_tooltip_window.c"
#include "editor.c"
#include "node.c"
#include "package.c"
#include "preferences.c"
#include "preferences_dialog.c"
#include "process.c"
#include "project_file.c"
#include "project_index.c"
#include "project.c"
#include "project_repl.c"
#include "repl_process.c"
#include "repl_session.c"
#include "reloc.c"
#include "status_bar.c"
#include "status_service.c"
#endif

#include "app.h"
#include "includes.h"
#include "preferences.h"
#include "process.h"
#include "repl_process.h"
#include "repl_session.h"
#include "status_service.h"
#include "util.h"

int
main (int argc, char *argv[])
{
  LOG(1, "Main.main");
  relocate();

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

