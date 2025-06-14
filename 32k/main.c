
#ifdef INLINE
#define STATIC static
#include "reloc.c"
#include "file_open.c"
#include "file_save.c"
#include "preferences.c"
#include "preferences_dialog.c"
#include "find_executables.c"
#include "process.c"
#include "swank_process.c"
#include "swank_session.c"
#include "evaluate.c"
#include "app.c"
#endif

#include "includes.h"
#include "app.h"
#include "process.h"
#include "swank_process.h"
#include "swank_session.h"
#include "preferences.h"

int
main (int argc, char *argv[])
{
  relocate();

  Preferences *prefs = preferences_new (g_get_user_config_dir ());

  ProcessImpl *proc = process_new (prefs);
  SwankProcessImpl *swank_proc = swank_process_new (proc, prefs);
  SwankSession *swank = swank_session_new (swank_proc);
  App *app     = app_new (prefs, swank);

  int status = g_application_run (G_APPLICATION (app), argc, argv);
  g_object_unref (app);
  g_object_unref (swank);
  g_object_unref (prefs);
  exit(status);
}

