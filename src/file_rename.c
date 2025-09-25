#include <gtk/gtk.h>
#include <glib/gstdio.h>
#include "file_rename.h"
#include "app.h"
#include "editor.h"
#include "project.h"
#include "project_file.h"
#include "asdf.h"

void file_rename(GtkWidget */*widget*/, gpointer data) {
  App *app = data;
  ProjectFile *file = app_get_current_file(app);
  if (!file)
    return;
  const gchar *old_path = project_file_get_path(file);
  if (!old_path)
    return;
  gchar *basename = g_path_get_basename(old_path);
  GtkWidget *dialog = gtk_dialog_new_with_buttons("Rename", NULL, 0,
      "_Cancel", GTK_RESPONSE_CANCEL,
      "_Refactor", GTK_RESPONSE_ACCEPT,
      NULL);
  GtkWidget *content = gtk_dialog_get_content_area(GTK_DIALOG(dialog));
  gchar *label_text = g_strdup_printf("Rename file '%s' and its usages to:", basename);
  GtkWidget *label = gtk_label_new(label_text);
  GtkWidget *entry = gtk_entry_new();
  gtk_entry_set_text(GTK_ENTRY(entry), basename);
  GtkWidget *box = gtk_box_new(GTK_ORIENTATION_VERTICAL, 6);
  gtk_box_pack_start(GTK_BOX(box), label, FALSE, FALSE, 0);
  gtk_box_pack_start(GTK_BOX(box), entry, FALSE, FALSE, 0);
  gtk_container_add(GTK_CONTAINER(content), box);
  gtk_widget_show_all(dialog);
  if (gtk_dialog_run(GTK_DIALOG(dialog)) == GTK_RESPONSE_ACCEPT) {
    const gchar *new_name = gtk_entry_get_text(GTK_ENTRY(entry));
    if (new_name && *new_name && g_strcmp0(new_name, basename) != 0) {
      gchar *dir = g_path_get_dirname(old_path);
      gchar *new_path = g_build_filename(dir, new_name, NULL);
      const gchar *old_rel = project_file_get_relative_path(file);
      gchar *old_base = g_path_get_basename(old_rel);
      GString *old_str = g_string_new(old_base);
      g_free(old_base);
      gchar *dot = g_strrstr(old_str->str, ".");
      if (dot)
        g_string_truncate(old_str, dot - old_str->str);
      if (g_rename(old_path, new_path) == 0) {
        project_file_set_path(file, new_path);
        const gchar *new_rel = project_file_get_relative_path(file);
        gchar *new_base = g_path_get_basename(new_rel);
        GString *new_str = g_string_new(new_base);
        g_free(new_base);
        dot = g_strrstr(new_str->str, ".");
        if (dot)
          g_string_truncate(new_str, dot - new_str->str);
        Asdf *asdf = project_get_asdf(app_get_project(app));
        asdf_rename_component(asdf, old_str, new_str);
        g_string_free(new_str, TRUE);
        asdf_save(asdf, asdf_get_filename(asdf));
        app_update_project_view(app);
        EditorContainer *notebook = app_get_notebook(app);
        if (notebook) {
          gint page = gtk_notebook_get_current_page(GTK_NOTEBOOK(notebook));
          GtkWidget *scrolled = gtk_notebook_get_nth_page(GTK_NOTEBOOK(notebook), page);
          GtkWidget *tab = gtk_notebook_get_tab_label(GTK_NOTEBOOK(notebook), scrolled);
          gtk_label_set_text(GTK_LABEL(tab), new_rel);
        }
      }
      g_free(dir);
      g_free(new_path);
      g_string_free(old_str, TRUE);
    }
  }
  gtk_widget_destroy(dialog);
  g_free(label_text);
  g_free(basename);
}

