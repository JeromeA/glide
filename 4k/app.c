#include <gtk/gtk.h>
#include <gtksourceview/gtksource.h>
#include <stdio.h>

void on_open_file(GtkWidget *, gpointer data) {
    GtkSourceBuffer *source_buffer = GTK_SOURCE_BUFFER(data);

    // Create a file chooser dialog
    GtkWidget *dialog = gtk_file_chooser_dialog_new(
        "Open File",
        NULL,
        GTK_FILE_CHOOSER_ACTION_OPEN,
        "_Cancel", GTK_RESPONSE_CANCEL,
        "_Open", GTK_RESPONSE_ACCEPT,
        NULL);

    if (gtk_dialog_run(GTK_DIALOG(dialog)) == GTK_RESPONSE_ACCEPT) {
        char *filename = gtk_file_chooser_get_filename(GTK_FILE_CHOOSER(dialog));

        // Open and read the file
        FILE *file = fopen(filename, "r");
        if (file) {
            fseek(file, 0, SEEK_END);
            long length = ftell(file);
            fseek(file, 0, SEEK_SET);

            char *content = g_malloc(length + 1);
            length = fread(content, 1, length, file);
            content[length] = '\0';
            fclose(file);

            // Set the content to the buffer
            gtk_text_buffer_set_text(GTK_TEXT_BUFFER(source_buffer), content, -1);
            g_free(content);
        } else {
            g_printerr("Failed to open file: %s\n", filename);
        }
        g_free(filename);
    }

    gtk_widget_destroy(dialog);
}

void on_save_file(GtkWidget *, gpointer data) {
  GtkSourceBuffer *source_buffer = GTK_SOURCE_BUFFER(data);
}

int main(int argc, char *argv[]) {
    // Initialize GTK
    gtk_init(&argc, &argv);

    // Create the main application window
    GtkWidget *window = gtk_window_new(GTK_WINDOW_TOPLEVEL);
    gtk_window_set_default_size(GTK_WINDOW(window), 800, 600);
    g_signal_connect(window, "delete-event", G_CALLBACK(gtk_main_quit), NULL);

    // Create a scrolled window
    GtkWidget *scrolled_window = gtk_scrolled_window_new(NULL, NULL);
    gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrolled_window), GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);

    // Create a GtkSourceBuffer with syntax highlighting
    GtkSourceLanguageManager *lang_manager = gtk_source_language_manager_get_default();
    GtkSourceLanguage *language = gtk_source_language_manager_get_language(lang_manager, "commonlisp");
    GtkSourceBuffer *source_buffer = gtk_source_buffer_new_with_language(language);

    // Add the GtkSourceView to the window
    GtkWidget *source_view = gtk_source_view_new_with_buffer(source_buffer);
    gtk_source_view_set_show_line_numbers(GTK_SOURCE_VIEW(source_view), TRUE);
    gtk_container_add(GTK_CONTAINER(scrolled_window), source_view);

    // Create a menu bar
    GtkWidget *menu_bar = gtk_menu_bar_new();
    GtkWidget *file_menu = gtk_menu_new();
    GtkWidget *file_menu_item = gtk_menu_item_new_with_label("File");
    GtkWidget *open_menu_item = gtk_menu_item_new_with_label("Open...");
    GtkWidget *save_menu_item = gtk_menu_item_new_with_label("Save...");
    GtkWidget *quit_menu_item = gtk_menu_item_new_with_label("Quit");

    gtk_menu_item_set_submenu(GTK_MENU_ITEM(file_menu_item), file_menu);
    gtk_menu_shell_append(GTK_MENU_SHELL(file_menu), open_menu_item);
    gtk_menu_shell_append(GTK_MENU_SHELL(file_menu), save_menu_item);
    gtk_menu_shell_append(GTK_MENU_SHELL(file_menu), quit_menu_item);
    gtk_menu_shell_append(GTK_MENU_SHELL(menu_bar), file_menu_item);

    // Connect the Open menu item to the callback
    g_signal_connect(open_menu_item, "activate", G_CALLBACK(on_open_file), source_buffer);
    g_signal_connect(save_menu_item, "activate", G_CALLBACK(on_save_file), source_buffer);
    g_signal_connect(quit_menu_item, "activate", G_CALLBACK(gtk_main_quit), NULL);

    // Create a vertical box to pack the menu and the scrolled window
    GtkWidget *vbox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
    gtk_box_pack_start(GTK_BOX(vbox), menu_bar, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox), scrolled_window, TRUE, TRUE, 0);
    gtk_container_add(GTK_CONTAINER(window), vbox);

    // Show all widgets
    gtk_widget_show_all(window);

    // Start the GTK main loop
    gtk_main();

    exit(0);
}

