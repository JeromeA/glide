
#ifdef INLINE
#define STATIC static
#include "reloc.c"
#include "file_open.c"
#include "file_save.c"
#include "preferences.c"
#include "preferences_dialog.c"
#include "find_executables.c"
#include "swank.c"
#include "evaluate.c"
#include "glide.c"
#endif

#include "includes.h"
#include "glide.h"

int
main (int argc, char *argv[])
{
  relocate();
  gchar *prefs_file = g_build_filename (g_get_user_config_dir (),
                                        "glide", "preferences.ini", NULL);

  Glide *app   = glide_new (prefs_file);
  g_free (prefs_file);

  int status = g_application_run (G_APPLICATION (app), argc, argv);
  g_object_unref (app);
  return status;
}

