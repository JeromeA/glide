
#ifndef PREFERENCES_DIALOG_H
#define PREFERENCES_DIALOG_H

#include <gtk/gtk.h> // For GtkWidget

// Changed signature: data is no longer needed as preferences are global.
void on_preferences_global(GtkWidget *widget);

#endif // PREFERENCES_DIALOG_H
