
#ifdef INLINE
#define STATIC static
#include "app.c"
#include "evaluate.c"
#include "file_open.c"
#include "file_save.c"
#include "find_executables.c"
#include "gtk_text_provider.c"
#include "interactions_view.c"
#include "lisp_lexer.c"
#include "package.c"
#include "package_common_lisp_user.c"
#include "package_common_lisp.c"
#include "node_info.c"
#include "lisp_parser.c"
#include "analyser.c"
#include "lisp_source_notebook.c"
#include "lisp_source_view.c"
#include "lisp_parser_view.c"
#include "preferences.c"
#include "preferences_dialog.c"
#include "process.c"
#include "project.c"
#include "real_process.c"
#include "real_swank_process.c"
#include "real_swank_session.c"
#include "reloc.c"
#include "string_text_provider.c"
#include "swank_process.c"
#include "swank_session.c"
#include "text_provider.c"
#undef g_debug
#define g_debug(...) do { } while (0)
#endif

#include "app.h"
#include "includes.h"
#include "preferences.h"
#include "process.h"
#include "real_process.h"
#include "real_swank_process.h"
#include "real_swank_session.h"
#include "swank_process.h"
#include "swank_session.h"
#include "project.h"

int
main (int argc, char *argv[])
{
  g_debug("Main.main");
  relocate();

  Preferences *prefs = preferences_new (g_get_user_config_dir ());

  const gchar *sdk_path = preferences_get_sdk (prefs);
  Process *proc = real_process_new (sdk_path);
  SwankProcess *swank_proc = real_swank_process_new (proc, prefs);
  SwankSession *swank = real_swank_session_new (swank_proc);
  Project *project = project_new();
  App *app     = app_new (prefs, swank, project);

  int status = g_application_run (G_APPLICATION (app), argc, argv);
  g_object_unref (app);
  g_object_unref (swank);
  g_object_unref (swank_proc);
  g_object_unref (proc);
  g_object_unref (project);
  g_object_unref (prefs);
  exit(status);
}

