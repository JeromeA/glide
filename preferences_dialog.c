
#include <gtk/gtk.h>
#include "preferences.h"
#include "find_executables.h"
#if __has_include("app.h")
# define WITH_APP
# include "app.h"
#endif

typedef struct {
  GtkWidget *dialog;
  GtkWidget *binary_combo_box;
  Preferences *preferences;
} PreferencesDialog;

PreferencesDialog *
preferences_dialog_new(GtkWindow *parent, Preferences *preferences) {
  PreferencesDialog *self = g_new0(PreferencesDialog, 1);
  self->preferences = preferences;

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
  g_ptr_array_free(executables, TRUE);

  gtk_grid_attach(GTK_GRID(grid), binary_label, 0, 0, 1, 1);
  gtk_grid_attach(GTK_GRID(grid), self->binary_combo_box, 1, 0, 1, 1);

  gtk_widget_show_all(self->dialog);

  return self;
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
    gboolean confirmed = gtk_dialog_run(GTK_DIALOG(prefs->dialog)) == GTK_RESPONSE_OK;
    if (confirmed) {
        preferences_dialog_save(prefs);
    }
    gtk_widget_hide(prefs->dialog);
    return confirmed;
}

void on_preferences(GtkWidget *widget, gpointer data) {
  GtkWindow *main_window = NULL;
  Preferences *preferences = NULL;
  main_window = GTK_WINDOW(gtk_widget_get_toplevel(widget));

#ifdef WITH_APP
  App *app = GLIDE_APP(data);
  preferences = app_get_preferences(app);
#else
  preferences = (Preferences *)data;
#endif

  PreferencesDialog *prefs = preferences_dialog_new(main_window, preferences);
  preferences_dialog_run(prefs);
  preferences_dialog_free(prefs);
}


