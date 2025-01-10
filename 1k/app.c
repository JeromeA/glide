#include <gtk/gtk.h>
#include "reloc.h"

#ifdef INLINE
#include "reloc.c"
#endif

int main(int argc, char *argv[]) {
  relocate();

  // Initialize GTK
  gtk_init(&argc, &argv);

  // Create the main application window
  GtkWidget *window = (GtkWidget*)gtk_window_new(GTK_WINDOW_TOPLEVEL);
  g_signal_connect(window, "delete-event", G_CALLBACK(gtk_main_quit), NULL);

  // Show all widgets
  gtk_widget_show_all(window);

  // Start the GTK main loop
  gtk_main();

  exit(0);
}

