#include <gtk/gtk.h>
#include "project_new_wizard.h"
#include "app.h"
#include "project.h"
#include "asdf.h"
#include <glib/gstdio.h>

typedef struct {
  GtkEntry *name_entry;
  GtkEntry *location_entry;
  GtkLabel *info_label;
} ProjectNewWidgets;

static void update_info(GtkEditable */*editable*/, gpointer user_data) {
  ProjectNewWidgets *w = user_data;
  const gchar *name = gtk_entry_get_text(w->name_entry);
  const gchar *loc = gtk_entry_get_text(w->location_entry);
  gchar *path = g_build_filename(loc, name, NULL);
  gchar *msg = g_strdup_printf("Project will be created in %s/", path);
  gtk_label_set_text(w->info_label, msg);
  g_free(msg);
  g_free(path);
}

static gchar *expand_home(const gchar *path) {
  if (path[0] == '~')
    return g_build_filename(g_get_home_dir(), path + 1, NULL);
  return g_strdup(path);
}

void project_new_wizard(GtkWidget */*widget*/, gpointer data) {
  App *app = data;
  GtkWidget *dialog = gtk_dialog_new_with_buttons("New Project", NULL,
      GTK_DIALOG_MODAL,
      "_Cancel", GTK_RESPONSE_CANCEL,
      "_Create", GTK_RESPONSE_ACCEPT,
      NULL);
  GtkWidget *content = gtk_dialog_get_content_area(GTK_DIALOG(dialog));
  GtkWidget *grid = gtk_grid_new();
  gtk_container_set_border_width(GTK_CONTAINER(grid), 6);
  gtk_grid_set_row_spacing(GTK_GRID(grid), 6);
  gtk_grid_set_column_spacing(GTK_GRID(grid), 6);
  gtk_container_add(GTK_CONTAINER(content), grid);

  GtkWidget *name_label = gtk_label_new("Name:");
  GtkWidget *name_entry = gtk_entry_new();
  gtk_entry_set_text(GTK_ENTRY(name_entry), "untitled");
  gtk_grid_attach(GTK_GRID(grid), name_label, 0, 0, 1, 1);
  gtk_grid_attach(GTK_GRID(grid), name_entry, 1, 0, 1, 1);

  GtkWidget *loc_label = gtk_label_new("Location:");
  GtkWidget *loc_entry = gtk_entry_new();
  Preferences *prefs = app_get_preferences(app);
  const gchar *dir = preferences_get_project_dir(prefs);
  gtk_entry_set_text(GTK_ENTRY(loc_entry), dir);
  gtk_grid_attach(GTK_GRID(grid), loc_label, 0, 1, 1, 1);
  gtk_grid_attach(GTK_GRID(grid), loc_entry, 1, 1, 1, 1);

  GtkWidget *info = gtk_label_new(NULL);
  gtk_widget_set_sensitive(info, FALSE);
  gtk_grid_attach(GTK_GRID(grid), info, 1, 2, 1, 1);

  ProjectNewWidgets w = {GTK_ENTRY(name_entry), GTK_ENTRY(loc_entry), GTK_LABEL(info)};
  g_signal_connect(name_entry, "changed", G_CALLBACK(update_info), &w);
  g_signal_connect(loc_entry, "changed", G_CALLBACK(update_info), &w);
  update_info(NULL, &w);

  gtk_widget_show_all(dialog);
  if (gtk_dialog_run(GTK_DIALOG(dialog)) == GTK_RESPONSE_ACCEPT) {
    const gchar *name = gtk_entry_get_text(GTK_ENTRY(name_entry));
    const gchar *loc_text = gtk_entry_get_text(GTK_ENTRY(loc_entry));
    preferences_set_project_dir(prefs, loc_text);
    gchar *loc = expand_home(loc_text);
    gchar *dir = g_build_filename(loc, name, NULL);
    g_mkdir_with_parents(dir, 0755);
    gchar *name_asd = g_strdup_printf("%s.asd", name);
    gchar *asd_path = g_build_filename(dir, name_asd, NULL);
    Asdf *asdf = asdf_new_from_file(asd_path);
    asdf_save(asdf, asd_path);
    project_set_asdf(app_get_project(app), asdf);
    g_object_unref(asdf);
    g_free(name_asd);
    g_free(asd_path);
    g_free(dir);
    g_free(loc);
  }
  gtk_widget_destroy(dialog);
}

