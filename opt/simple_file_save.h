#ifndef OPT_SIMPLE_FILE_SAVE_H
#define OPT_SIMPLE_FILE_SAVE_H

#include <gtk/gtk.h> // For GtkWidget

// Operates on global filename_global and buffer_global
void simple_file_save_global(GtkWidget *triggering_widget);
void simple_file_saveas_global(GtkWidget *triggering_widget);

#endif // OPT_SIMPLE_FILE_SAVE_H
