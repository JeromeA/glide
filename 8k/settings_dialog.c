
#include <gtk/gtk.h>

GtkWidget *create_settings_dialog(GtkWindow *parent) {
  GtkWidget *dialog = gtk_dialog_new_with_buttons("Settings",
      parent,
      GTK_DIALOG_MODAL,
      "_Close",
      GTK_RESPONSE_CLOSE,
      NULL);
  GtkWidget *content_area = gtk_dialog_get_content_area(GTK_DIALOG(dialog));

  GtkWidget *grid = gtk_grid_new();
  gtk_widget_set_margin_start(grid, 10);
  gtk_widget_set_margin_end(grid, 10);
  gtk_widget_set_margin_top(grid, 10);
  gtk_widget_set_margin_bottom(grid, 10);
  gtk_grid_set_row_spacing(GTK_GRID(grid), 10);
  gtk_grid_set_column_spacing(GTK_GRID(grid), 10);
  gtk_container_add(GTK_CONTAINER(content_area), grid);

  GtkWidget *binary_label = gtk_label_new("Lisp binary:");
  GtkWidget *binary_combo_box = gtk_combo_box_text_new();
  gtk_combo_box_text_append(GTK_COMBO_BOX_TEXT(binary_combo_box), "sbcl", "SBCL");
  gtk_combo_box_text_append(GTK_COMBO_BOX_TEXT(binary_combo_box), "clisp", "CLisp");
  gtk_combo_box_set_active_id(GTK_COMBO_BOX(binary_combo_box), "sbcl");

  gtk_grid_attach(GTK_GRID(grid), binary_label, 0, 0, 1, 1);
  gtk_grid_attach(GTK_GRID(grid), binary_combo_box, 1, 0, 1, 1);

  gtk_widget_show_all(dialog);
  return dialog;
}

void on_settings(GtkWidget *, gpointer data) {
  GtkWindow *main_window = GTK_WINDOW(data);
  GtkWidget *settings_dialog = create_settings_dialog(main_window);
  gtk_dialog_run(GTK_DIALOG(settings_dialog));
  gtk_widget_destroy(settings_dialog);
}


