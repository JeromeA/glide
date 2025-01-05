#include <gtk/gtk.h>
#include <gtksourceview/gtksource.h>
#include <stdio.h>
#include <dlfcn.h>

#if RELOC

// Each entry in the relocation table has a 48 byte overhead (in addition to the
// name of the symbol). So, even for a small program, removing a few symbols from
// the relocation table saves a lot of bytes.

#define D(sym) struct { unsigned char len; char text[sizeof(#sym)]; } f ## sym
#define I(sym) .f ## sym = { sizeof(#sym) + 1, #sym }
 
struct {
  D(exit);
  D(fclose);
  D(fopen);
  D(fread);
  D(fseek);
  D(ftell);
  D(fwrite);
  D(g_free);
  D(g_malloc);
  D(g_printerr);
  D(gtk_box_new);
  D(gtk_box_pack_start);
  D(gtk_container_add);
  D(gtk_dialog_run);
  D(gtk_file_chooser_dialog_new);
  D(gtk_file_chooser_get_filename);
  D(gtk_file_chooser_set_do_overwrite_confirmation);
  D(gtk_init);
  D(gtk_main);
  D(gtk_main_quit);
  D(gtk_menu_bar_new);
  D(gtk_menu_item_new_with_label);
  D(gtk_menu_item_set_submenu);
  D(gtk_menu_new);
  D(gtk_menu_shell_append);
  D(gtk_scrolled_window_new);
  D(gtk_scrolled_window_set_policy);
  D(gtk_source_buffer_new_with_language);
  D(gtk_source_language_manager_get_default);
  D(gtk_source_language_manager_get_language);
  D(gtk_source_view_new_with_buffer);
  D(gtk_source_view_set_show_line_numbers);
  D(gtk_text_buffer_get_end_iter);
  D(gtk_text_buffer_get_start_iter);
  D(gtk_text_buffer_get_text);
  D(gtk_text_buffer_set_text);
  D(gtk_widget_destroy);
  D(gtk_widget_show_all);
  D(gtk_window_new);
  D(gtk_window_set_default_size);
} reloc = {
  I(exit),
  I(fclose),
  I(fopen),
  I(fread),
  I(fseek),
  I(ftell),
  I(fwrite),
  I(g_free),
  I(g_malloc),
  I(g_printerr),
  I(gtk_box_new),
  I(gtk_box_pack_start),
  I(gtk_container_add),
  I(gtk_dialog_run),
  I(gtk_file_chooser_dialog_new),
  I(gtk_file_chooser_get_filename),
  I(gtk_file_chooser_set_do_overwrite_confirmation),
  I(gtk_init),
  I(gtk_main),
  I(gtk_main_quit),
  I(gtk_menu_bar_new),
  I(gtk_menu_item_new_with_label),
  I(gtk_menu_item_set_submenu),
  I(gtk_menu_new),
  I(gtk_menu_shell_append),
  I(gtk_scrolled_window_new),
  I(gtk_scrolled_window_set_policy),
  I(gtk_source_buffer_new_with_language),
  I(gtk_source_language_manager_get_default),
  I(gtk_source_language_manager_get_language),
  I(gtk_source_view_new_with_buffer),
  I(gtk_source_view_set_show_line_numbers),
  I(gtk_text_buffer_get_end_iter),
  I(gtk_text_buffer_get_start_iter),
  I(gtk_text_buffer_get_text),
  I(gtk_text_buffer_set_text),
  I(gtk_widget_destroy),
  I(gtk_widget_show_all),
  I(gtk_window_new),
  I(gtk_window_set_default_size),
};

long (*f[40])();

#define exit f[0]
#define fclose f[1]
#define fopen (FILE *)f[2]
#define fread f[3]
#define fseek f[4]
#define ftell f[5]
#define fwrite f[6]
// #define g_free f[7]
#define g_malloc (char *)f[8]
#define g_printerr f[9]
// #define gtk_box_new f[10]
#define gtk_box_pack_start f[11]
#define gtk_container_add f[12]
#define gtk_dialog_run f[13]
#define gtk_file_chooser_dialog_new (GtkWidget *)f[14]
#define gtk_file_chooser_get_filename (gchar *)f[15]
#define gtk_file_chooser_set_do_overwrite_confirmation f[16]
#define gtk_init f[17]
#define gtk_main f[18]
#define gtk_main_quit f[19]
#define gtk_menu_bar_new (GtkWidget *)f[20]
#define gtk_menu_item_new_with_label (GtkWidget *)f[21]
#define gtk_menu_item_set_submenu f[22]
#define gtk_menu_new (GtkWidget *)f[23]
#define gtk_menu_shell_append f[24]
#define gtk_scrolled_window_new (GtkWidget*)f[25]
#define gtk_scrolled_window_set_policy f[26]
// #define gtk_source_buffer_new_with_language f[27]
#define gtk_source_language_manager_get_default (GtkSourceLanguageManager*)f[28]
#define gtk_source_language_manager_get_language (GtkSourceLanguage*)f[29]
#define gtk_source_view_new_with_buffer (GtkWidget*)f[30]
#define gtk_source_view_set_show_line_numbers f[31]
#define gtk_text_buffer_get_end_iter f[32]
#define gtk_text_buffer_get_start_iter f[33]
#define gtk_text_buffer_get_text (char *)f[34]
#define gtk_text_buffer_set_text f[35]
#define gtk_widget_destroy f[36]
#define gtk_widget_show_all f[37]
#define gtk_window_new (GtkWidget *)f[38]
#define gtk_window_set_default_size f[39]

#endif

gchar *filename = NULL;

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
    g_free(filename);
    filename = gtk_file_chooser_get_filename(GTK_FILE_CHOOSER(dialog));

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
  }

  gtk_widget_destroy(dialog);
}

void on_save_file(GtkWidget *, gpointer data) {
  GtkSourceBuffer *source_buffer = GTK_SOURCE_BUFFER(data);

  // Check if we already have a filename
  if (!filename) {
    // We do not have a known filename -> use a "Save As" dialog
    GtkWidget *dialog = gtk_file_chooser_dialog_new(
        "Save File",
        NULL,
        GTK_FILE_CHOOSER_ACTION_SAVE,
        "_Cancel", GTK_RESPONSE_CANCEL,
        "_Save", GTK_RESPONSE_ACCEPT,
        NULL);

    // Suggest a name or remember the last directory, etc.
    gtk_file_chooser_set_do_overwrite_confirmation(GTK_FILE_CHOOSER(dialog), TRUE);

    if (gtk_dialog_run(GTK_DIALOG(dialog)) == GTK_RESPONSE_ACCEPT) {
      gchar *chosen_filename = gtk_file_chooser_get_filename(GTK_FILE_CHOOSER(dialog));
      // Store that filename in our global variable
      if (filename) {
        g_free(filename);
      }
      filename = chosen_filename; // Now we own chosen_filename
    }

    gtk_widget_destroy(dialog);

    // If the user canceled, filename is still NULL
    if (!filename) {
      return; // bail out
    }
  }

  // At this point, we have a valid filename (either from opening a file or from Save As).
  // Get the text from the buffer
  GtkTextIter start, end;
  gtk_text_buffer_get_start_iter(GTK_TEXT_BUFFER(source_buffer), &start);
  gtk_text_buffer_get_end_iter(GTK_TEXT_BUFFER(source_buffer), &end);

  gchar *buffer_text = gtk_text_buffer_get_text(GTK_TEXT_BUFFER(source_buffer), &start, &end, FALSE);

  // Write the text to the file
  FILE *file = fopen(filename, "w");
  if (!file) {
    g_printerr("Failed to open file for writing: %s\n", filename);
  } else {
    fwrite(buffer_text, sizeof(gchar), strlen(buffer_text), file);
    fclose(file);
  }

  g_free(buffer_text);
}

void on_saveas_file(GtkWidget *, gpointer data) {
  char * old_filename = filename;
  filename = NULL;
  on_save_file(NULL, data);
  if (filename) {
    g_free(old_filename);
  } else {
    filename = old_filename;
  }
}

int main(int argc, char *argv[]) {

#if RELOC
  char *p = (char *)&reloc;
  for(int i=0 ; i<40 ; i++) {
    f[i] = dlsym(RTLD_DEFAULT, p+1);
#if DEBUG
    if (!f[i]) {
      printf("dlsym: %s\n", dlerror());
      exit(1);
    }
#endif
    p += *p;
  }
#endif

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
  GtkWidget *save_menu_item = gtk_menu_item_new_with_label("Save");
  GtkWidget *saveas_menu_item = gtk_menu_item_new_with_label("Save as...");
  GtkWidget *quit_menu_item = gtk_menu_item_new_with_label("Quit");

  gtk_menu_item_set_submenu(GTK_MENU_ITEM(file_menu_item), file_menu);
  gtk_menu_shell_append(GTK_MENU_SHELL(file_menu), open_menu_item);
  gtk_menu_shell_append(GTK_MENU_SHELL(file_menu), save_menu_item);
  gtk_menu_shell_append(GTK_MENU_SHELL(file_menu), saveas_menu_item);
  gtk_menu_shell_append(GTK_MENU_SHELL(file_menu), quit_menu_item);
  gtk_menu_shell_append(GTK_MENU_SHELL(menu_bar), file_menu_item);

  // Connect the Open menu item to the callback
  g_signal_connect(open_menu_item, "activate", G_CALLBACK(on_open_file), source_buffer);
  g_signal_connect(save_menu_item, "activate", G_CALLBACK(on_save_file), source_buffer);
  g_signal_connect(saveas_menu_item, "activate", G_CALLBACK(on_saveas_file), source_buffer);
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

