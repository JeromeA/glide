
#ifdef INLINE
#define STATIC static
#include "analyse.c"
#include "analyse_defpackage.c"
#include "analyse_defun.c"
#include "actions.c"
#include "menu_bar.c"
#include "app.c"
#include "asdf.c"
#include "asdf_view.c"
#include "evaluate.c"
#include "function.c"
#include "file_open.c"
#include "file_new.c"
#include "file_add.c"
#include "file_rename.c"
#include "file_delete.c"
#include "project_new_wizard.c"
#include "file_save.c"
#include "gtk_text_provider.c"
#include "interactions_view.c"
#include "lisp_lexer.c"
#include "lisp_parser.c"
#include "lisp_parser_view.c"
#include "lisp_source_notebook.c"
#include "editor.c"
#include "node.c"
#include "package.c"
#include "package_common_lisp.c"
#include "package_common_lisp_user.c"
#include "preferences.c"
#include "preferences_dialog.c"
#include "process.c"
#include "project_file.c"
#include "project.c"
#include "glide_process.c"
#include "glide_session.c"
#include "reloc.c"
#include "status_bar.c"
#include "status_service.c"
#include "string_text_provider.c"
#include "text_provider.c"
#undef g_debug
#define g_debug(...) do { } while (0)
#endif

#include "app.h"
#include "includes.h"
#include "preferences.h"
#include "process.h"
#include "glide_process.h"
#include "glide_session.h"
#include "project.h"
#include "status_service.h"

int
main (int argc, char *argv[])
{
  g_debug("Main.main");
  relocate();

  Preferences *prefs = preferences_new (g_get_user_config_dir ());

  const gchar *sdk_path = preferences_get_sdk (prefs);
  Process *proc = process_new (sdk_path);
  GlideProcess *glide_proc = glide_process_new (proc);
  StatusService *status_service = status_service_new();
  GlideSession *glide = glide_session_new (glide_proc, status_service);
  Project *project = project_new();
  App *app     = app_new (prefs, glide, project, status_service);

  int status = g_application_run (G_APPLICATION (app), argc, argv);
  g_object_unref(app);
  glide_session_unref(glide);
  glide_process_unref(glide_proc);
  process_unref(proc);
  project_unref(project);
  status_service_free(status_service);
  preferences_unref(prefs);
  exit(status);
}

