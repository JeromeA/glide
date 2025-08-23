
#include <gtk/gtk.h>
#include "preferences.h"
#if __has_include("app.h")
# define WITH_APP
# include "app.h"
#endif

typedef struct {
  GtkWidget *dialog;
  GtkWidget *binary_entry;
  Preferences *preferences;
} PreferencesDialog;

static void
on_choose_binary(GtkButton * /*button*/, gpointer data) {
  PreferencesDialog *self = data;
  GtkWidget *dialog = gtk_file_chooser_dialog_new("Select Lisp Binary",
    GTK_WINDOW(self->dialog), GTK_FILE_CHOOSER_ACTION_OPEN,
    "_Cancel", GTK_RESPONSE_CANCEL,
    "_Open", GTK_RESPONSE_ACCEPT,
    NULL);
  if (gtk_dialog_run(GTK_DIALOG(dialog)) == GTK_RESPONSE_ACCEPT) {
    gchar *filename = gtk_file_chooser_get_filename(GTK_FILE_CHOOSER(dialog));
    gtk_entry_set_text(GTK_ENTRY(self->binary_entry), filename);
    g_free(filename);
  }
  gtk_widget_destroy(dialog);
}

static void
on_install_sbcl(GtkButton * /*button*/, gpointer /*data*/) {
  const gchar *argv[] = {
    "/usr/bin/x-terminal-emulator",
    "-e",
    "sudo apt install sbcl",
    NULL
  };
  g_spawn_async(NULL, (gchar **)argv, NULL, G_SPAWN_SEARCH_PATH, NULL, NULL, NULL, NULL);
}

static void
on_copy_install(GtkButton * /*button*/, gpointer data) {
  GtkWidget *install_entry = data;
  GtkClipboard *clipboard = gtk_clipboard_get(GDK_SELECTION_CLIPBOARD);
  const gchar *text = gtk_entry_get_text(GTK_ENTRY(install_entry));
  gtk_clipboard_set_text(clipboard, text, -1);
}

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
  self->binary_entry = gtk_entry_new();
  GtkWidget *choose_button = gtk_button_new_with_label("...");
  g_signal_connect(choose_button, "clicked", G_CALLBACK(on_choose_binary), self);

  gtk_grid_attach(GTK_GRID(grid), binary_label, 0, 0, 1, 1);
  gtk_grid_attach(GTK_GRID(grid), self->binary_entry, 1, 0, 1, 1);
  gtk_grid_attach(GTK_GRID(grid), choose_button, 2, 0, 1, 1);

  GtkWidget *install_label = gtk_label_new("To install SBCL, run:");
  GtkWidget *install_entry = gtk_entry_new();
  gtk_entry_set_text(GTK_ENTRY(install_entry), "sudo apt install sbcl");
  gtk_editable_set_editable(GTK_EDITABLE(install_entry), FALSE);
  GtkWidget *copy_button = gtk_button_new_from_icon_name("edit-copy", GTK_ICON_SIZE_BUTTON);
  g_signal_connect(copy_button, "clicked", G_CALLBACK(on_copy_install), install_entry);
  gtk_grid_attach(GTK_GRID(grid), install_label, 0, 1, 1, 1);
  gtk_grid_attach(GTK_GRID(grid), install_entry, 1, 1, 1, 1);
  gtk_grid_attach(GTK_GRID(grid), copy_button, 2, 1, 1, 1);

  GtkWidget *install_button = gtk_button_new_with_label("Run installer for SBCL");
  g_signal_connect(install_button, "clicked", G_CALLBACK(on_install_sbcl), NULL);
  gtk_grid_attach(GTK_GRID(grid), install_button, 1, 2, 2, 1);

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
    gtk_entry_set_text(GTK_ENTRY(prefs->binary_entry), current_sdk ? current_sdk : "");

    gtk_widget_show_all(prefs->dialog);
}

void preferences_dialog_save(PreferencesDialog *prefs) {
  const gchar *selected_sdk = gtk_entry_get_text(GTK_ENTRY(prefs->binary_entry));
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


