#include "menu_bar.h"
#include "actions.h"
#include "file_open.h"
#include "file_new.h"
#include "file_add.h"
#include "file_rename.h"
#include "file_delete.h"
#include "project_new_wizard.h"
#include "preferences_dialog.h"
#include "evaluate.h"

GtkWidget *
menu_bar_new(App *self)
{
  GtkWidget *menu_bar      = gtk_menu_bar_new();
  GtkWidget *file_menu     = gtk_menu_new();
  GtkWidget *file_item     = gtk_menu_item_new_with_label("File");

  GtkWidget *edit_menu     = gtk_menu_new();
  GtkWidget *edit_item     = gtk_menu_item_new_with_label("Edit");
  GtkWidget *extend_item   = gtk_menu_item_new_with_label("Extend selection");
  GtkWidget *shrink_item   = gtk_menu_item_new_with_label("Shrink selection");

  GtkWidget *refactor_menu = gtk_menu_new();
  GtkWidget *refactor_item = gtk_menu_item_new_with_label("Refactor");
  GtkWidget *refactor_file_menu = gtk_menu_new();
  GtkWidget *refactor_file_item = gtk_menu_item_new_with_label("File");
  GtkWidget *rename_item   = gtk_menu_item_new_with_label("Rename");
  GtkWidget *delete_item   = gtk_menu_item_new_with_label("Delete");

  GtkWidget *run_menu      = gtk_menu_new();
  GtkWidget *run_item      = gtk_menu_item_new_with_label("Run");
  GtkWidget *eval_item     = gtk_menu_item_new_with_label("Eval toplevel");

  GtkWidget *project_menu  = gtk_menu_new();
  GtkWidget *project_item  = gtk_menu_item_new_with_label("Project");
  GtkWidget *proj_new_item = gtk_menu_item_new_with_label("New…");
  GtkWidget *proj_open_item = gtk_menu_item_new_with_label("Open…");
  GtkWidget *proj_recent_item = gtk_menu_item_new_with_label("Recent");
  GtkWidget *recent_menu   = gtk_menu_new();
  app_set_recent_menu(self, recent_menu);

  GtkWidget *newfile_item  = gtk_menu_item_new_with_label("New file");
  GtkWidget *addfile_item  = gtk_menu_item_new_with_label("Add file");
  GtkWidget *saveall_item  = gtk_menu_item_new_with_label("Save all");
  GtkWidget *closeproj_item = gtk_menu_item_new_with_label("Close project");
  GtkWidget *settings_item = gtk_menu_item_new_with_label("Settings…");
  GtkWidget *exit_item     = gtk_menu_item_new_with_label("Exit");

  gtk_menu_item_set_submenu(GTK_MENU_ITEM(file_item), file_menu);
  gtk_menu_item_set_submenu(GTK_MENU_ITEM(project_item), project_menu);
  gtk_menu_item_set_submenu(GTK_MENU_ITEM(proj_recent_item), recent_menu);
  gtk_menu_shell_append(GTK_MENU_SHELL(project_menu), proj_new_item);
  gtk_menu_shell_append(GTK_MENU_SHELL(project_menu), proj_open_item);
  gtk_menu_shell_append(GTK_MENU_SHELL(project_menu), proj_recent_item);
  gtk_menu_shell_append(GTK_MENU_SHELL(file_menu), project_item);
  gtk_menu_shell_append(GTK_MENU_SHELL(file_menu), newfile_item);
  gtk_menu_shell_append(GTK_MENU_SHELL(file_menu), addfile_item);
  gtk_menu_shell_append(GTK_MENU_SHELL(file_menu), saveall_item);
  gtk_menu_shell_append(GTK_MENU_SHELL(file_menu), closeproj_item);
  gtk_menu_shell_append(GTK_MENU_SHELL(file_menu), gtk_separator_menu_item_new());
  gtk_menu_shell_append(GTK_MENU_SHELL(file_menu), settings_item);
  gtk_menu_shell_append(GTK_MENU_SHELL(file_menu), gtk_separator_menu_item_new());
  gtk_menu_shell_append(GTK_MENU_SHELL(file_menu), exit_item);
  gtk_menu_shell_append(GTK_MENU_SHELL(menu_bar), file_item);
  gtk_menu_item_set_submenu(GTK_MENU_ITEM(edit_item), edit_menu);
  gtk_menu_shell_append(GTK_MENU_SHELL(edit_menu), extend_item);
  gtk_menu_shell_append(GTK_MENU_SHELL(edit_menu), shrink_item);
  gtk_menu_shell_append(GTK_MENU_SHELL(menu_bar), edit_item);
  gtk_menu_item_set_submenu(GTK_MENU_ITEM(refactor_item), refactor_menu);
  gtk_menu_item_set_submenu(GTK_MENU_ITEM(refactor_file_item), refactor_file_menu);
  gtk_menu_shell_append(GTK_MENU_SHELL(refactor_file_menu), rename_item);
  gtk_menu_shell_append(GTK_MENU_SHELL(refactor_file_menu), delete_item);
  gtk_menu_shell_append(GTK_MENU_SHELL(refactor_menu), refactor_file_item);
  gtk_menu_shell_append(GTK_MENU_SHELL(menu_bar), refactor_item);

  gtk_menu_item_set_submenu(GTK_MENU_ITEM(run_item), run_menu);
  gtk_menu_shell_append(GTK_MENU_SHELL(run_menu), eval_item);
  gtk_menu_shell_append(GTK_MENU_SHELL(menu_bar), run_item);

  g_signal_connect(proj_new_item, "activate", G_CALLBACK(project_new_wizard), self);
  g_signal_connect(proj_open_item, "activate", G_CALLBACK(file_open), self);
  g_signal_connect(newfile_item, "activate", G_CALLBACK(file_new), self);
  g_signal_connect(addfile_item, "activate", G_CALLBACK(file_add), self);
  g_signal_connect(saveall_item, "activate", G_CALLBACK(on_save_all), self);
  g_signal_connect(closeproj_item, "activate", G_CALLBACK(on_close_project), self);
  g_signal_connect(settings_item, "activate", G_CALLBACK(on_preferences), self);
  g_signal_connect(exit_item, "activate", G_CALLBACK(on_quit_menu), self);
  g_signal_connect(rename_item, "activate", G_CALLBACK(file_rename), self);
  g_signal_connect(delete_item, "activate", G_CALLBACK(file_delete), self);
  g_signal_connect(extend_item, "activate", G_CALLBACK(on_extend_selection), self);
  g_signal_connect(shrink_item, "activate", G_CALLBACK(on_shrink_selection), self);
  g_signal_connect(eval_item, "activate", G_CALLBACK(on_evaluate), self);

  return menu_bar;
}
