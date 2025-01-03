#include <gtk/gtk.h>
#include <dlfcn.h>

// Each entry in the relocation table has a 48 byte overhead (in addition to the
// name of the symbol). So, even for this simple program, removing 5 symbols from
// the relocation table and adding them with dlsym still saves 109 bytes.

#define D(sym) struct { unsigned char len; char text[sizeof(#sym)]; } f ## sym
#define I(sym) .f ## sym = { sizeof(#sym) + 1, #sym }

struct {
  D(gtk_window_new);
  D(gtk_main_quit);
  D(gtk_widget_show_all);
  D(gtk_main);
  D(exit);
} reloc = {
  I(gtk_window_new),
  I(gtk_main_quit),
  I(gtk_widget_show_all),
  I(gtk_main),
  I(exit),
};

long (*f[5])();

#define gtk_window_new f[0]
#define gtk_main_quit f[1]
#define gtk_widget_show_all f[2]
#define gtk_main f[3]
#define exit f[4]

int main(int argc, char *argv[]) {
  char *p = (char *)&reloc;
  for(int i=0 ; i<5 ; i++) {
    f[i] = dlsym(RTLD_DEFAULT, p+1);
    p += *p;
  }

  // Initialize GTK
  gtk_init(&argc, &argv);

  // Create the main application window
  GtkWidget *window = (GtkWidget*)gtk_window_new(GTK_WINDOW_TOPLEVEL);
  g_signal_connect(window, "delete-event", G_CALLBACK(gtk_main_quit), NULL);

  // Show all widgets
  gtk_widget_show_all(window);

  // Start the GTK main loop
  gtk_main();

  exit(0);
}

