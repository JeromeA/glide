#include <gtk/gtk.h>
#include <gtksourceview/gtksource.h>
#include <dlfcn.h>
#include "reloc.h"

#ifdef INLINE
#include "reloc.c"
#endif

int main(int argc, char *argv[]) {
  relocate();

  // Initialize GTK
  gtk_init(&argc, &argv);

  // Create the main application window
  GtkWidget *window = (GtkWidget *)gtk_window_new(GTK_WINDOW_TOPLEVEL);
  gtk_window_set_default_size(GTK_WINDOW(window), 800, 600);
  g_signal_connect(window, "delete-event", G_CALLBACK(gtk_main_quit), NULL);

  // Create a scrolled window
  GtkWidget *scrolled_window = gtk_scrolled_window_new(NULL, NULL);
  gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrolled_window), GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
  gtk_container_add(GTK_CONTAINER(window), scrolled_window);

  // Create a GtkSourceBuffer with syntax highlighting
  GtkSourceLanguageManager *lang_manager = gtk_source_language_manager_get_default();
  GtkSourceLanguage *language = gtk_source_language_manager_get_language(lang_manager, "commonlisp");
  GtkSourceBuffer *source_buffer = gtk_source_buffer_new_with_language(language);

  // Add the GtkSourceView to the window
  GtkWidget *source_view = gtk_source_view_new_with_buffer(source_buffer);
  gtk_source_view_set_show_line_numbers(GTK_SOURCE_VIEW(source_view), TRUE);
  gtk_container_add(GTK_CONTAINER(scrolled_window), source_view);

  // Show all widgets
  gtk_widget_show_all(window);

  // Start the GTK main loop
  gtk_main();

  exit(0);
}

