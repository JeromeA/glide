#include <gtk/gtk.h>
#include "project_new_wizard.h"
#include "app.h"
#include "project.h"
#include "asdf.h"
#include "file_open.h"
#include <glib/gstdio.h>
#include "util.h"

typedef struct {
  GtkEntry *name_entry;
  GtkEntry *location_entry;
  GtkLabel *info_label;
  GtkWidget *create_button;
  const gchar *base_dir;
  gboolean location_dirty;
} ProjectNewWidgets;

static void update_info(ProjectNewWidgets *w);
static void name_changed(GtkEditable *editable, gpointer user_data);
static void location_changed(GtkEditable *editable, gpointer user_data);

static gchar *expand_home(const gchar *path) {
  if (path[0] == '~')
    return g_build_filename(g_get_home_dir(), path + 1, NULL);
  return g_strdup(path);
}

static void update_info(ProjectNewWidgets *w) {
  const gchar *name = gtk_entry_get_text(w->name_entry);
  const gchar *loc = gtk_entry_get_text(w->location_entry);
  gchar *loc_expanded = expand_home(loc);
  gchar *name_asd = g_strdup_printf("%s.asd", name);
  gchar *asd = g_build_filename(loc_expanded, name_asd, NULL);
  if (g_file_test(asd, G_FILE_TEST_EXISTS)) {
    gchar *msg = g_strdup_printf("Already a project named %s/%s", loc, name_asd);
    gtk_label_set_text(w->info_label, msg);
    gtk_widget_set_sensitive(w->create_button, FALSE);
    g_free(msg);
  } else {
    gtk_widget_set_sensitive(w->create_button, TRUE);
    if (g_file_test(loc_expanded, G_FILE_TEST_IS_DIR)) {
      gchar *msg = g_strdup_printf("Project will be created in existing folder %s", loc);
      gtk_label_set_text(w->info_label, msg);
      g_free(msg);
    } else {
      gchar *msg = g_strdup_printf("Project will be created in the new folder %s", loc);
      gtk_label_set_text(w->info_label, msg);
      g_free(msg);
    }
  }
  g_free(asd);
  g_free(name_asd);
  g_free(loc_expanded);
}

static void location_changed(GtkEditable */*editable*/, gpointer user_data) {
  ProjectNewWidgets *w = user_data;
  w->location_dirty = TRUE;
  update_info(w);
}

static void name_changed(GtkEditable */*editable*/, gpointer user_data) {
  ProjectNewWidgets *w = user_data;
  if (!w->location_dirty) {
    const gchar *name = gtk_entry_get_text(w->name_entry);
    gchar *loc = g_build_filename(w->base_dir, name, NULL);
    g_signal_handlers_block_by_func(w->location_entry, location_changed, w);
    gtk_entry_set_text(w->location_entry, loc);
    g_signal_handlers_unblock_by_func(w->location_entry, location_changed, w);
    g_free(loc);
  }
  update_info(w);
}

void project_new_wizard(GtkWidget */*widget*/, gpointer data) {
  g_debug("project_new_wizard");
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
  gchar *initial = g_build_filename(dir, "untitled", NULL);
  gtk_entry_set_text(GTK_ENTRY(loc_entry), initial);
  g_free(initial);
  gtk_grid_attach(GTK_GRID(grid), loc_label, 0, 1, 1, 1);
  gtk_grid_attach(GTK_GRID(grid), loc_entry, 1, 1, 1, 1);

  GtkWidget *info = gtk_label_new(NULL);
  gtk_widget_set_sensitive(info, FALSE);
  gtk_grid_attach(GTK_GRID(grid), info, 1, 2, 1, 1);

  GtkWidget *create_button = gtk_dialog_get_widget_for_response(GTK_DIALOG(dialog), GTK_RESPONSE_ACCEPT);
  ProjectNewWidgets w = {GTK_ENTRY(name_entry), GTK_ENTRY(loc_entry), GTK_LABEL(info), create_button, dir, FALSE};
  g_signal_connect(name_entry, "changed", G_CALLBACK(name_changed), &w);
  g_signal_connect(loc_entry, "changed", G_CALLBACK(location_changed), &w);
  update_info(&w);

  gtk_widget_show_all(dialog);
  if (gtk_dialog_run(GTK_DIALOG(dialog)) == GTK_RESPONSE_ACCEPT) {
    g_debug("project_new_wizard accepted");
    const gchar *name = gtk_entry_get_text(GTK_ENTRY(name_entry));
    const gchar *loc_text = gtk_entry_get_text(GTK_ENTRY(loc_entry));
    gchar *pref_dir = g_path_get_dirname(loc_text);
    preferences_set_project_dir(prefs, pref_dir);
    g_free(pref_dir);
    gchar *dir = expand_home(loc_text);
    g_debug("creating project in %s", dir);
    g_mkdir_with_parents(dir, 0755);
    gchar *unnamed = g_build_filename(dir, "unnamed.lisp", NULL);
    g_file_set_contents(unnamed, "", -1, NULL);
    gchar *name_asd = g_strdup_printf("%s.asd", name);
    gchar *asd_path = g_build_filename(dir, name_asd, NULL);
    Asdf *asdf = asdf_new_from_file(asd_path);
    asdf_add_component(asdf, "unnamed");
    asdf_save(asdf, asd_path);
    g_object_unref(asdf);
    file_open_path(app, asd_path);
    g_free(unnamed);
    g_free(name_asd);
    g_free(asd_path);
    g_free(dir);
  }
  gtk_widget_destroy(dialog);
}

