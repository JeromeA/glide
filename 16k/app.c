#include "includes.h"
#include "glide.h"

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
#endif

int
main (int argc, char *argv[])
{
  relocate();
  Glide *app   = glide_new ();
  int status = g_application_run (G_APPLICATION (app), argc, argv);
  g_object_unref (app);
  return status;
}

