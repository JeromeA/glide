
#ifdef INLINE
#define STATIC static
#include "reloc.c"
#include "file_open.c"
#include "file_save.c"
#include "preferences.c"
#include "preferences_dialog.c"
#include "find_executables.c"
#include "process.c"
#include "real_process.c"
#include "swank_process.c"
#include "real_swank_process.c"
#include "swank_session.c"
#include "evaluate.c"
#include "app.c"
#endif

#include "includes.h"
#include "app.h"
#include "process.h"
#include "real_process.h"
#include "swank_process.h"
#include "real_swank_process.h"
#include "swank_session.h"
#include "preferences.h"

int
main (int argc, char *argv[])
{
  g_debug("Main.main");
  relocate();

  Preferences *prefs = preferences_new (g_get_user_config_dir ());

  const gchar *sdk_path = preferences_get_sdk (prefs);
  Process *proc = real_process_new (sdk_path);
  SwankProcess *swank_proc = real_swank_process_new (proc, prefs);
  SwankSession *swank = swank_session_new (swank_proc);
  App *app     = app_new (prefs, swank);

  int status = g_application_run (G_APPLICATION (app), argc, argv);
  g_object_unref (app);
  g_object_unref (swank);
  g_object_unref (prefs);
  exit(status);
}

