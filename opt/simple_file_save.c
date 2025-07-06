#include "simple_file_save.h" // Own header
#include <gtk/gtk.h>
#include <gtksourceview/gtksource.h> // For GtkSourceBuffer
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <errno.h>
#include <string.h>   // For strlen
#include "reloc.h"
#include "syscalls.h"

// Global variables defined in main.c
extern GtkSourceBuffer *buffer_global;
// filename_global is managed via main_get_filename and main_set_filename
extern const gchar* main_get_filename();
extern void main_set_filename(const gchar *new_filename);


void simple_file_save_global(GtkWidget *triggering_widget) {
    GtkWindow *parent_window = NULL;
    if (triggering_widget) {
        parent_window = GTK_WINDOW(gtk_widget_get_toplevel(triggering_widget));
    }

    if (!buffer_global) {
        g_printerr("simple_file_save_global: buffer_global is NULL. Cannot save file.\n");
        return;
    }

    const gchar *current_filename = main_get_filename();
    gchar *filename_to_save = NULL;

    if (!current_filename) {
        GtkWidget *dialog = gtk_file_chooser_dialog_new(
            "Save File As",
            parent_window,
            GTK_FILE_CHOOSER_ACTION_SAVE,
            "_Cancel", GTK_RESPONSE_CANCEL,
            "_Save", GTK_RESPONSE_ACCEPT,
            NULL);
        gtk_file_chooser_set_do_overwrite_confirmation(GTK_FILE_CHOOSER(dialog), TRUE);
        // gtk_file_chooser_set_current_name(GTK_FILE_CHOOSER(dialog), "Untitled"); // Optional

        if (gtk_dialog_run(GTK_DIALOG(dialog)) == GTK_RESPONSE_ACCEPT) {
            filename_to_save = gtk_file_chooser_get_filename(GTK_FILE_CHOOSER(dialog));
            main_set_filename(filename_to_save); // Update global filename
        }
        gtk_widget_destroy(dialog);

        if (!filename_to_save) { // User cancelled Save As
            return;
        }
    } else {
        filename_to_save = g_strdup(current_filename); // Use existing global filename
    }

    GtkTextIter start, end;
    gtk_text_buffer_get_start_iter(GTK_TEXT_BUFFER(buffer_global), &start);
    gtk_text_buffer_get_end_iter(GTK_TEXT_BUFFER(buffer_global), &end);
    gchar *buffer_text = gtk_text_buffer_get_text(GTK_TEXT_BUFFER(buffer_global), &start, &end, FALSE);

    int fd = sys_open(filename_to_save, O_WRONLY | O_CREAT | O_TRUNC, 0666);
    if (fd == -1) {
        g_printerr("Failed to open file %s for writing (errno %d)\n", filename_to_save, errno);
        g_free(buffer_text);
        g_free(filename_to_save); // Free if strdup'd or from chooser
        return;
    }

    size_t to_write = strlen(buffer_text);
    ssize_t total_written = 0;
    while ((size_t)total_written < to_write) { // Cast total_written to size_t for comparison
        ssize_t written = sys_write(fd, buffer_text + total_written, to_write - total_written);
        if (written == -1) {
            g_printerr("Error writing to %s (errno %d)\n", filename_to_save, errno);
            sys_close(fd);
            g_free(buffer_text);
            g_free(filename_to_save);
            return;
        }
        total_written += written;
    }

    if (sys_close(fd) == -1) {
        g_printerr("Error closing %s (errno %d)\n", filename_to_save, errno);
    }

    g_free(buffer_text);
    g_free(filename_to_save); // Free if strdup'd or from chooser
}

void simple_file_saveas_global(GtkWidget *triggering_widget) {
    // Store current filename, then set global to NULL to force Save As dialog in simple_file_save_global
    gchar *original_filename_backup = NULL;
    const gchar* current_global_filename = main_get_filename();
    if (current_global_filename) {
        original_filename_backup = g_strdup(current_global_filename);
    }

    main_set_filename(NULL); // Force simple_file_save_global to open "Save As" dialog

    simple_file_save_global(triggering_widget);

    // If save was cancelled, main_get_filename() would still be NULL (or whatever new name if saved).
    // If it's NULL (cancelled), restore original.
    if (main_get_filename() == NULL && original_filename_backup != NULL) {
        main_set_filename(original_filename_backup);
    }
    g_free(original_filename_backup);
}
