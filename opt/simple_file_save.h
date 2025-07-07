#ifndef OPT_SIMPLE_FILE_SAVE_H
#define OPT_SIMPLE_FILE_SAVE_H

#include <gtk/gtk.h> // For GtkWidget

// Operates on filename and buffer (via main_get_filename, main_set_filename, main_get_source_buffer)
void simple_file_save_ui(GtkWidget *triggering_widget);
void simple_file_saveas_ui(GtkWidget *triggering_widget);

#endif // OPT_SIMPLE_FILE_SAVE_H
