
#ifdef INLINE
#define STATIC static
#include "reloc.c"
#include "file_open.c"
#include "file_save.c"
#include "preferences.c"
#include "preferences_dialog.c"
#include "find_executables.c"
#include "app.c"
#endif

#include "includes.h"
#include "app.h"
#include "preferences.h"

int
main (int argc, char *argv[])
{
  relocate();

  Preferences *prefs = preferences_new (g_get_user_config_dir ());

  App *app     = app_new (prefs);

  int status = g_application_run (G_APPLICATION (app), argc, argv);
  g_object_unref (app);
  g_object_unref (prefs);
  exit(status);
}

