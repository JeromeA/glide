#ifndef OPT_SIMPLE_FILE_OPEN_H
#define OPT_SIMPLE_FILE_OPEN_H

#include <gtk/gtk.h> // For GtkWidget

// Operates on buffer and filename (via main_get_filename, main_set_filename)
void simple_file_open_ui(GtkWidget *triggering_widget);

#endif // OPT_SIMPLE_FILE_OPEN_H
