
#include <gtk/gtk.h>
#include "preferences.h"

typedef struct {
  GtkWidget *dialog;
  GtkWidget *binary_combo_box;
  Preferences *preferences;
} PreferencesDialog;

PreferencesDialog *preferences_dialog_new(GtkWindow *parent) {
  PreferencesDialog *prefs = g_new0(PreferencesDialog, 1);
  prefs->preferences = preferences_get_instance();

  prefs->dialog = gtk_dialog_new_with_buttons(
      "Preferences", parent, GTK_DIALOG_MODAL,
      "_OK", GTK_RESPONSE_OK,
      "_Cancel", GTK_RESPONSE_CANCEL,
      NULL
      );

  GtkWidget *content_area = gtk_dialog_get_content_area(GTK_DIALOG(prefs->dialog));

  GtkWidget *grid = gtk_grid_new();
  gtk_widget_set_margin_start(grid, 10);
  gtk_widget_set_margin_end(grid, 10);
  gtk_widget_set_margin_top(grid, 10);
  gtk_widget_set_margin_bottom(grid, 10);
  gtk_grid_set_row_spacing(GTK_GRID(grid), 10);
  gtk_grid_set_column_spacing(GTK_GRID(grid), 10);
  gtk_container_add(GTK_CONTAINER(content_area), grid);

  GtkWidget *binary_label = gtk_label_new("Lisp binary:");
  prefs->binary_combo_box = gtk_combo_box_text_new();
  gtk_combo_box_text_append(GTK_COMBO_BOX_TEXT(prefs->binary_combo_box), "sbcl", "SBCL");
  gtk_combo_box_text_append(GTK_COMBO_BOX_TEXT(prefs->binary_combo_box), "clisp", "CLisp");

  gtk_grid_attach(GTK_GRID(grid), binary_label, 0, 0, 1, 1);
  gtk_grid_attach(GTK_GRID(grid), prefs->binary_combo_box, 1, 0, 1, 1);

  gtk_widget_show_all(prefs->dialog);

  return prefs;
}

void preferences_dialog_free(PreferencesDialog *prefs) {
  gtk_widget_destroy(prefs->dialog);
  g_free(prefs);
}

void preferences_dialog_load(PreferencesDialog *prefs) {
    // Set current value from preferences
    const gchar *current_sdk = preferences_get_sdk(prefs->preferences);
    gtk_combo_box_set_active_id(GTK_COMBO_BOX(prefs->binary_combo_box), current_sdk);

    gtk_widget_show_all(prefs->dialog);
}

void preferences_dialog_save(PreferencesDialog *prefs) {
  const gchar *selected_sdk = gtk_combo_box_get_active_id(GTK_COMBO_BOX(prefs->binary_combo_box));
  preferences_set_sdk(prefs->preferences, selected_sdk);
}

gboolean preferences_dialog_run(PreferencesDialog *prefs) {
    preferences_dialog_load(prefs);

    gboolean result = (gtk_dialog_run(GTK_DIALOG(prefs->dialog)) == GTK_RESPONSE_OK);
    if (result) {
        preferences_dialog_save(prefs);
    }
    gtk_widget_hide(prefs->dialog);
    return result;
}

void on_preferences(GtkWidget *, gpointer data) {
  GtkWindow *main_window = GTK_WINDOW(data);
  PreferencesDialog *prefs = preferences_dialog_new(main_window);
  preferences_dialog_run(prefs);
  preferences_dialog_free(prefs);
}


