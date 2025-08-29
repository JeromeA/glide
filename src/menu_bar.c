#include "menu_bar.h"

GtkWidget *
menu_bar_new(App *self)
{
  GMenu *menu_bar = g_menu_new();

  GMenu *file_menu = g_menu_new();
  GMenu *project_menu = g_menu_new();
  g_menu_append(project_menu, "New…", "app.project-new");
  g_menu_append(project_menu, "Open…", "app.project-open");
  GMenu *recent_menu = g_menu_new();
  g_menu_append_submenu(project_menu, "Recent", G_MENU_MODEL(recent_menu));
  app_set_recent_menu(self, recent_menu);
  g_menu_append(project_menu, "Close project", "app.close-project");
  g_menu_append_section(file_menu, NULL, G_MENU_MODEL(project_menu));

  GMenu *file_actions = g_menu_new();
  g_menu_append(file_actions, "New file", "app.file-new");
  g_menu_append(file_actions, "Add file", "app.file-add");
  g_menu_append(file_actions, "Save all", "app.save-all");
  g_menu_append_section(file_menu, NULL, G_MENU_MODEL(file_actions));

  GMenu *settings_menu = g_menu_new();
  g_menu_append(settings_menu, "Settings…", "app.preferences");
  g_menu_append_section(file_menu, NULL, G_MENU_MODEL(settings_menu));

  GMenu *exit_menu = g_menu_new();
  g_menu_append(exit_menu, "Exit", "app.quit");
  g_menu_append_section(file_menu, NULL, G_MENU_MODEL(exit_menu));

  g_menu_append_submenu(menu_bar, "File", G_MENU_MODEL(file_menu));

  GMenu *edit_menu = g_menu_new();
  g_menu_append(edit_menu, "Extend selection", "app.extend-selection");
  g_menu_append(edit_menu, "Shrink selection", "app.shrink-selection");
  g_menu_append_submenu(menu_bar, "Edit", G_MENU_MODEL(edit_menu));

  GMenu *refactor_menu = g_menu_new();
  GMenu *refactor_file_menu = g_menu_new();
  g_menu_append(refactor_file_menu, "Rename", "app.file-rename");
  g_menu_append(refactor_file_menu, "Delete", "app.file-delete");
  g_menu_append_submenu(refactor_menu, "File", G_MENU_MODEL(refactor_file_menu));
  g_menu_append_submenu(menu_bar, "Refactor", G_MENU_MODEL(refactor_menu));

  GMenu *run_menu = g_menu_new();
  g_menu_append(run_menu, "Eval toplevel", "app.eval-toplevel");
  g_menu_append(run_menu, "Eval selection", "app.eval-selection");
  g_menu_append_submenu(menu_bar, "Run", G_MENU_MODEL(run_menu));

  GtkWidget *widget = gtk_menu_bar_new_from_model(G_MENU_MODEL(menu_bar));
  g_object_unref(menu_bar);
  g_object_unref(file_menu);
  g_object_unref(project_menu);
  g_object_unref(file_actions);
  g_object_unref(settings_menu);
  g_object_unref(exit_menu);
  g_object_unref(edit_menu);
  g_object_unref(refactor_menu);
  g_object_unref(refactor_file_menu);
  g_object_unref(run_menu);

  return widget;
}

