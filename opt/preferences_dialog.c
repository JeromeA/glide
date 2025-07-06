
#include <gtk/gtk.h>
#include "preferences.h" // For global preference functions
#include "find_executables.h"
#include "preferences_dialog.h" // For on_preferences_global (self include)

// App related includes and defines are removed as App class is gone.
// #if __has_include("app.h")
// # define WITH_APP
// # include "app.h"
// #endif

typedef struct {
  GtkWidget *dialog;
  GtkWidget *binary_combo_box;
  // Preferences *preferences; // Removed, preferences are global
} PreferencesDialog;

PreferencesDialog *
preferences_dialog_new(GtkWindow *parent) { // `Preferences *preferences` argument removed
  PreferencesDialog *self = g_new0(PreferencesDialog, 1);
  // self->preferences = preferences; // Removed

  self->dialog = gtk_dialog_new_with_buttons(
      "Preferences", parent, GTK_DIALOG_MODAL,
      "_OK", GTK_RESPONSE_OK,
      "_Cancel", GTK_RESPONSE_CANCEL,
      NULL
      );

  GtkWidget *content_area = gtk_dialog_get_content_area(GTK_DIALOG(self->dialog));

  GtkWidget *grid = gtk_grid_new();
  gtk_widget_set_margin_start(grid, 10);
  gtk_widget_set_margin_end(grid, 10);
  gtk_widget_set_margin_top(grid, 10);
  gtk_widget_set_margin_bottom(grid, 10);
  gtk_grid_set_row_spacing(GTK_GRID(grid), 10);
  gtk_grid_set_column_spacing(GTK_GRID(grid), 10);
  gtk_container_add(GTK_CONTAINER(content_area), grid);

  GtkWidget *binary_label = gtk_label_new("Lisp binary:");
  self->binary_combo_box = gtk_combo_box_text_new();
  GPtrArray *executables = find_lisp_executables();
  for (guint i = 0; i < executables->len; i++) {
    const gchar *executable = g_ptr_array_index(executables, i);
    gtk_combo_box_text_append(GTK_COMBO_BOX_TEXT(self->binary_combo_box), executable, executable);
  }
  g_ptr_array_free(executables, TRUE); // Free the array and its contents (char*)

  gtk_grid_attach(GTK_GRID(grid), binary_label, 0, 0, 1, 1);
  gtk_grid_attach(GTK_GRID(grid), self->binary_combo_box, 1, 0, 1, 1);

  // gtk_widget_show_all(self->dialog); // Show is usually done by the caller or run function
  return self;
}

void preferences_dialog_free(PreferencesDialog *prefs_dialog) {
  if (prefs_dialog) {
    gtk_widget_destroy(prefs_dialog->dialog);
    g_free(prefs_dialog);
  }
}

void preferences_dialog_load(PreferencesDialog *prefs_dialog) {
    // Set current value from global preferences
    const gchar *current_sdk = preferences_get_sdk_global();
    gtk_combo_box_set_active_id(GTK_COMBO_BOX(prefs_dialog->binary_combo_box), current_sdk);
    // gtk_widget_show_all(prefs_dialog->dialog); // Not needed here, run will show it
}

void preferences_dialog_save(PreferencesDialog *prefs_dialog) {
  const gchar *selected_sdk = gtk_combo_box_get_active_id(GTK_COMBO_BOX(prefs_dialog->binary_combo_box));
  preferences_set_sdk_global(selected_sdk);
}

gboolean preferences_dialog_run(PreferencesDialog *prefs_dialog) {
    preferences_dialog_load(prefs_dialog);
    gtk_widget_show_all(prefs_dialog->dialog); // Show the dialog before running it
    gboolean confirmed = gtk_dialog_run(GTK_DIALOG(prefs_dialog->dialog)) == GTK_RESPONSE_OK;
    if (confirmed) {
        preferences_dialog_save(prefs_dialog);
    }
    gtk_widget_hide(prefs_dialog->dialog); // Hide after run, free is separate
    return confirmed;
}

// Renamed and signature changed
void on_preferences_global(GtkWidget *widget) {
  GtkWindow *main_window = GTK_WINDOW(gtk_widget_get_toplevel(widget));

  // Preferences are no longer passed around or fetched from App
  // PreferencesDialog *prefs_dialog = preferences_dialog_new(main_window, preferences);
  PreferencesDialog *prefs_dialog = preferences_dialog_new(main_window);
  preferences_dialog_run(prefs_dialog);
  preferences_dialog_free(prefs_dialog);
}


