#include "simple_file_open.h" // Own header
#include <gtk/gtk.h>
#include <gtksourceview/gtksource.h> // For GtkSourceBuffer
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <errno.h>
#include "reloc.h"    // For relocate() - though not used directly here, often in main
#include "syscalls.h" // For sys_open, sys_read, sys_close, sys_fstat

// Global variables defined in main.c
extern GtkSourceBuffer *buffer_global;
extern void main_set_filename(const gchar *new_filename); // Defined in main.c


void simple_file_open_global(GtkWidget *triggering_widget) {
    GtkWindow *parent_window = NULL;
    if (triggering_widget) {
        parent_window = GTK_WINDOW(gtk_widget_get_toplevel(triggering_widget));
    }

    if (!buffer_global) {
        g_printerr("simple_file_open_global: buffer_global is NULL. Cannot open file.\n");
        return;
    }

    GtkWidget *dialog = gtk_file_chooser_dialog_new(
        "Open File",
        parent_window,
        GTK_FILE_CHOOSER_ACTION_OPEN,
        "_Cancel", GTK_RESPONSE_CANCEL,
        "_Open",   GTK_RESPONSE_ACCEPT,
        NULL);

    if (gtk_dialog_run(GTK_DIALOG(dialog)) == GTK_RESPONSE_ACCEPT) {
        gchar *chosen_filename = gtk_file_chooser_get_filename(GTK_FILE_CHOOSER(dialog));
        
        // Use main_set_filename to update the global filename
        main_set_filename(chosen_filename);
        // filename_global is now updated by main_set_filename.

        int fd = sys_open(chosen_filename, O_RDONLY, 0);
        if (fd == -1) {
            g_printerr("Failed to open %s (errno %d)\n", chosen_filename, errno);
        } else {
            struct stat sb;
            if (sys_fstat(fd, &sb) == -1) {
                g_printerr("Failed to stat %s (errno %d)\n", chosen_filename, errno);
                sys_close(fd);
            } else if (!S_ISREG(sb.st_mode)) {
                g_printerr("Not a regular file: %s\n", chosen_filename);
                sys_close(fd);
            } else {
                off_t length = sb.st_size;
                gchar *content = g_malloc(length + 1);
                if (!content) {
                    g_printerr("Failed to allocate memory for file content.\n");
                    sys_close(fd);
                } else {
                    ssize_t total_read = 0;
                    while (total_read < length) {
                        ssize_t r = sys_read(fd, content + total_read, length - total_read);
                        if (r == -1) {
                            g_printerr("Error reading %s (errno %d)\n", chosen_filename, errno);
                            break;
                        } else if (r == 0) {
                            break; // EOF
                        }
                        total_read += r;
                    }
                    content[total_read] = '\0';
                    sys_close(fd);

                    gtk_text_buffer_set_text(GTK_TEXT_BUFFER(buffer_global), content, -1);
                    g_free(content);
                }
            }
        }
        g_free(chosen_filename); // Allocated by gtk_file_chooser_get_filename
    }

    gtk_widget_destroy(dialog);
}
