#include <gtk/gtk.h>
#include <gtksourceview/gtksource.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <errno.h>
#include "reloc.h"
#include "syscalls.h"
#include "file_open.h"

extern gchar *filename;

void file_open(GtkWidget *, gpointer data) {
    GtkSourceBuffer *source_buffer = GTK_SOURCE_BUFFER(data);

    // Create a file chooser dialog
    GtkWidget *dialog = gtk_file_chooser_dialog_new(
        "Open File",
        NULL,
        GTK_FILE_CHOOSER_ACTION_OPEN,
        "_Cancel", GTK_RESPONSE_CANCEL,
        "_Open",   GTK_RESPONSE_ACCEPT,
        NULL);

    if (gtk_dialog_run(GTK_DIALOG(dialog)) == GTK_RESPONSE_ACCEPT) {
        g_free(filename);
        filename = gtk_file_chooser_get_filename(GTK_FILE_CHOOSER(dialog));
        
        // Open the file using syscalls
        int fd = sys_open(filename, O_RDONLY, 0);
        if (fd == -1) {
            // Handle error opening file
            g_printerr("Failed to open %s (errno %d)\n",
                       filename, errno);
        } else {
            struct stat sb;
            if (sys_fstat(fd, &sb) == -1) {
                g_printerr("Failed to stat %s (errno %d)\n",
                           filename, errno);
                sys_close(fd);
            } else if (!S_ISREG(sb.st_mode)) {
                g_printerr("Not a regular file: %s\n", filename);
                sys_close(fd);
            } else {
                // sb.st_size is the size of the file in bytes
                off_t length = sb.st_size;
                
                // Allocate buffer for file content + null terminator
                char *content = g_malloc(length + 1);
                if (!content) {
                    g_printerr("Failed to allocate memory.\n");
                    sys_close(fd);
                } else {
                    ssize_t total_read = 0;
                    
                    // Read loop to handle partial reads
                    while (total_read < length) {
                        ssize_t r = sys_read(fd, content + total_read,
                                         length - total_read);
                        if (r == -1) {
                            g_printerr("Error reading %s (errno %d)\n",
                                       filename, errno);
                            break;
                        } else if (r == 0) {
                            // EOF reached unexpectedly
                            break;
                        }
                        total_read += r;
                    }
                    
                    // Null-terminate properly in case we didn’t read the entire file
                    content[total_read] = '\0';
                    
                    // Close the file
                    sys_close(fd);

                    // Set the content to the buffer
                    gtk_text_buffer_set_text(GTK_TEXT_BUFFER(source_buffer),
                                             content, -1);
                    g_free(content);
                }
            }
        }
    }

    // Destroy the dialog
    gtk_widget_destroy(dialog);
}
