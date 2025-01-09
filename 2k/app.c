#include <gtk/gtk.h>
#include <gtksourceview/gtksource.h>
#include <dlfcn.h>

// Each entry in the relocation table has a 48 byte overhead (in addition to the
// name of the symbol). So, even for this simple program, removing 5 symbols from
// the relocation table and adding them with dlsym still saves 109 bytes.

#define D(sym) struct { unsigned char len; char text[sizeof(#sym)]; } f ## sym
#define I(sym) .f ## sym = { sizeof(#sym) + 1, #sym }
 
struct {
  D(exit);
  D(gtk_container_add);
  D(gtk_init);
  D(gtk_main);
  D(gtk_main_quit);
  D(gtk_scrolled_window_new);
  D(gtk_scrolled_window_set_policy);
  D(gtk_source_buffer_new_with_language);
  D(gtk_source_language_manager_get_default);
  D(gtk_source_language_manager_get_language);
  D(gtk_source_view_new_with_buffer);
  D(gtk_source_view_set_show_line_numbers);
  D(gtk_widget_show_all);
  D(gtk_window_new);
  D(gtk_window_set_default_size);
} reloc = {
  I(exit),
  I(gtk_container_add),
  I(gtk_init),
  I(gtk_main),
  I(gtk_main_quit),
  I(gtk_scrolled_window_new),
  I(gtk_scrolled_window_set_policy),
  I(gtk_source_buffer_new_with_language),
  I(gtk_source_language_manager_get_default),
  I(gtk_source_language_manager_get_language),
  I(gtk_source_view_new_with_buffer),
  I(gtk_source_view_set_show_line_numbers),
  I(gtk_widget_show_all),
  I(gtk_window_new),
  I(gtk_window_set_default_size)
};

long (*f[15])();

#define exit f[0]
#define gtk_container_add f[1]
// #define gtk_init f[2]
#define gtk_main f[3]
#define gtk_main_quit f[4]
#define gtk_scrolled_window_new (GtkWidget*)f[5]
#define gtk_scrolled_window_set_policy f[6]
// #define gtk_source_buffer_new_with_language f[7]
#define gtk_source_language_manager_get_default (GtkSourceLanguageManager*)f[8]
#define gtk_source_language_manager_get_language (GtkSourceLanguage*)f[9]
#define gtk_source_view_new_with_buffer (GtkWidget*)f[10]
#define gtk_source_view_set_show_line_numbers f[11]
#define gtk_widget_show_all f[12]
#define gtk_window_new f[13]
#define gtk_window_set_default_size f[14]

int main(int argc, char *argv[]) {
  char *p = (char *)&reloc;
  for(int i=0 ; i<15 ; i++) {
    f[i] = dlsym(RTLD_DEFAULT, p+1);
//    if (!f[i]) {
//      printf("dlsym: %s\n", dlerror());
//      exit(1);
//    }
    p += *p;
  }

  // Initialize GTK
  gtk_init(&argc, &argv);

  // Create the main application window
  GtkWidget *window = (GtkWidget *)gtk_window_new(GTK_WINDOW_TOPLEVEL);
  gtk_window_set_default_size(GTK_WINDOW(window), 800, 600);
  g_signal_connect(window, "delete-event", G_CALLBACK(gtk_main_quit), NULL);

  // Create a scrolled window
  GtkWidget *scrolled_window = gtk_scrolled_window_new(NULL, NULL);
  gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrolled_window), GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
  gtk_container_add(GTK_CONTAINER(window), scrolled_window);

  // Create a GtkSourceBuffer with syntax highlighting
  GtkSourceLanguageManager *lang_manager = gtk_source_language_manager_get_default();
  GtkSourceLanguage *language = gtk_source_language_manager_get_language(lang_manager, "commonlisp");
  GtkSourceBuffer *source_buffer = gtk_source_buffer_new_with_language(language);

  // Add the GtkSourceView to the window
  GtkWidget *source_view = gtk_source_view_new_with_buffer(source_buffer);
  gtk_source_view_set_show_line_numbers(GTK_SOURCE_VIEW(source_view), TRUE);
  gtk_container_add(GTK_CONTAINER(scrolled_window), source_view);

  // Show all widgets
  gtk_widget_show_all(window);

  // Start the GTK main loop
  gtk_main();

  exit(0);
}

