#include "includes.h"
#include "swank_process.h"
#include "swank_session.h"

// For file operations
#include "simple_file_open.h"
#include "simple_file_save.h"
#include "evaluate.h"
#include "interactions_view.h"


#ifdef INLINE
#define STATIC static
#include "evaluate.c"
#include "simple_file_open.c"
#include "simple_file_save.c"
#include "interactions_view.c"
#include "swank_process.c"
#include "swank_session.c"
#include "reloc.c"
#endif

// Global variables
GtkSourceBuffer *buffer_global = NULL;
gchar           *filename_global = NULL;
GtkWidget* interactions_view_global = NULL; // For SwankSession to call updates


// Forward declarations for signal handlers
static gboolean quit_delete_event_handler(GtkWidget *widget, GdkEvent *event, gpointer data);
static void quit_menu_item_handler(GtkWidget *item, gpointer data);
static gboolean on_key_press_handler(GtkWidget *widget, GdkEventKey *event, gpointer user_data);
void on_evaluate_global();


const gchar *main_get_filename() {
    return filename_global;
}

void main_set_filename(const gchar *new_filename) {
    g_debug("main_set_filename %s", new_filename ? new_filename : "(null)");
    gchar *dup = new_filename ? g_strdup(new_filename) : NULL;
    g_free(filename_global);
    filename_global = dup;
}

GtkSourceBuffer *main_get_source_buffer() {
    return buffer_global;
}

static void app_quit_global() {
  g_debug("app_quit_global");
  gtk_main_quit();
}

static void app_on_quit_global() {
  g_debug("app_on_quit_global");
  app_quit_global();
}

static gboolean quit_delete_event_handler(GtkWidget * /*widget*/, GdkEvent * /*event*/, gpointer /*data*/) {
  g_debug("quit_delete_event_handler");
  app_on_quit_global();
  return TRUE;
}

static void quit_menu_item_handler(GtkWidget * /*item*/, gpointer /*data*/) {
  g_debug("quit_menu_item_handler");
  app_on_quit_global();
}

static gboolean on_key_press_handler(GtkWidget * /*widget*/, GdkEventKey *event, gpointer /*user_data*/) {
  if ((event->keyval == GDK_KEY_Return) && (event->state & GDK_MOD1_MASK)) {
    on_evaluate_global();
    return TRUE;
  }
  return FALSE;
}


int main(int argc, char *argv[]) {
  g_debug("Main.main");
  relocate();
  gtk_init(&argc, &argv);

  // Initialize global Process
  process_init_globals("/usr/bin/sbcl");

  // Initialize global SwankProcess
  swank_process_init_globals();

  // Initialize global SwankSession
  swank_session_init_globals();

  // --- UI Setup ---
  GtkWidget *window = gtk_window_new(GTK_WINDOW_TOPLEVEL);
  gtk_window_set_default_size(GTK_WINDOW(window), 800, 600);
  g_signal_connect(window, "delete-event", G_CALLBACK(quit_delete_event_handler), NULL);

  GtkWidget *scrolled = gtk_scrolled_window_new(NULL, NULL);
  gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrolled), GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);

  GtkSourceLanguageManager *lm = gtk_source_language_manager_get_default();
  GtkSourceLanguage *lang = gtk_source_language_manager_get_language(lm, "commonlisp");
  buffer_global = gtk_source_buffer_new_with_language(lang);

  GtkWidget *view = gtk_source_view_new_with_buffer(buffer_global);
  gtk_source_view_set_show_line_numbers(GTK_SOURCE_VIEW(view), TRUE);
  gtk_container_add(GTK_CONTAINER(scrolled), view);

  g_signal_connect(view, "key-press-event", G_CALLBACK(on_key_press_handler), NULL);

  GtkWidget *menu_bar = gtk_menu_bar_new();
  GtkWidget *file_menu = gtk_menu_new();
  GtkWidget *file_item = gtk_menu_item_new_with_label("File");
  GtkWidget *open_item = gtk_menu_item_new_with_label("Open…");
  GtkWidget *save_item = gtk_menu_item_new_with_label("Save");
  GtkWidget *saveas_item = gtk_menu_item_new_with_label("Save as…");
  GtkWidget *quit_item = gtk_menu_item_new_with_label("Quit");

  gtk_menu_item_set_submenu(GTK_MENU_ITEM(file_item), file_menu);
  gtk_menu_shell_append(GTK_MENU_SHELL(file_menu), open_item);
  gtk_menu_shell_append(GTK_MENU_SHELL(file_menu), save_item);
  gtk_menu_shell_append(GTK_MENU_SHELL(file_menu), saveas_item);
  gtk_menu_shell_append(GTK_MENU_SHELL(file_menu), quit_item);
  gtk_menu_shell_append(GTK_MENU_SHELL(menu_bar), file_item);

  // Connect to new global file operations
  g_signal_connect(open_item, "activate", G_CALLBACK(simple_file_open_global), open_item);
  g_signal_connect(save_item, "activate", G_CALLBACK(simple_file_save_global), save_item);
  g_signal_connect(saveas_item, "activate", G_CALLBACK(simple_file_saveas_global), saveas_item);
  g_signal_connect(quit_item, "activate", G_CALLBACK(quit_menu_item_handler), NULL);

  // Store the created InteractionsView globally for RealSwankSession to use.
  interactions_view_global = GTK_WIDGET(interactions_view_new());
  GtkWidget *paned = gtk_paned_new(GTK_ORIENTATION_VERTICAL);
  gtk_paned_pack1(GTK_PANED(paned), scrolled, TRUE, TRUE);
  gtk_paned_pack2(GTK_PANED(paned), interactions_view_global, FALSE, TRUE);

  GtkWidget *vbox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
  gtk_box_pack_start(GTK_BOX(vbox), menu_bar, FALSE, FALSE, 0);
  gtk_box_pack_start(GTK_BOX(vbox), paned, TRUE, TRUE, 0);
  gtk_container_add(GTK_CONTAINER(window), vbox);

  gtk_widget_show_all(window);

  gtk_main();

  exit(0);
}
