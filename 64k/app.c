#include "includes.h"
#include "app.h"
#include "file_open.h"
#include "file_save.h"
#include "preferences_dialog.h"
#include "evaluate.h"
#include "interactions_view.h"
#include "lisp_source_view.h"
#include "lisp_parser_view.h"

/* Signal handlers */
STATIC gboolean quit_delete_event (GtkWidget * /*widget*/, GdkEvent * /*event*/, gpointer data);
STATIC void     quit_menu_item   (GtkWidget * /*item*/,   gpointer data);

/* === Instance structure ================================================= */
struct _App
{
  GtkApplication  parent_instance;

  /* UI pointers we want to reuse */
  GtkWidget      *window;
  GtkSourceBuffer*buffer;
  gchar          *filename;   /* current file path or NULL */
  Preferences    *preferences;
  SwankSession   *swank;
};

static void
on_show_parser(App *self)
{
  GtkWidget *win = gtk_window_new(GTK_WINDOW_TOPLEVEL);
  GtkWidget *view = lisp_parser_view_new(self->buffer);
  gtk_container_add(GTK_CONTAINER(win), view);
  gtk_widget_show_all(win);
}

static gboolean
on_key_press(GtkWidget * /*widget*/,
    GdkEventKey *event,
    gpointer     user_data)   /* actually App* */
{
  App *self = (App *) user_data;

  if ((event->keyval == GDK_KEY_Return) &&
      (event->state  & GDK_MOD1_MASK))      /* Alt+Enter */
  {
    on_evaluate(self);
    return TRUE;                  /* stop further propagation */
  }
  if ((event->keyval == GDK_KEY_p || event->keyval == GDK_KEY_P) &&
      (event->state & GDK_MOD1_MASK))        /* Alt+P */
  {
    on_show_parser(self);
    return TRUE;
  }
  return FALSE;
}


/* === GObject boiler-plate ============================================== */
G_DEFINE_TYPE(App, app, GTK_TYPE_APPLICATION)

/* ---  class_init ------------------------------------------------------- */
static void
app_activate (GApplication *app)
{
  App *self = GLIDE_APP(app);

  g_debug("App.activate");

  /*--------------------------------------------------------------*
   *  Build the UI (this is almost a verbatim move from app.c)    *
   *--------------------------------------------------------------*/
  self->window = gtk_application_window_new (GTK_APPLICATION (app));
  gtk_window_set_default_size (GTK_WINDOW (self->window), 800, 600);
  g_signal_connect (self->window, "delete-event",
                    G_CALLBACK (quit_delete_event), self);

  /* Scrolled source-view */
  GtkWidget *scrolled = gtk_scrolled_window_new (NULL, NULL);
  gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (scrolled), GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);

  GtkWidget *view = lisp_source_view_new ();
  self->buffer = lisp_source_view_get_buffer (LISP_SOURCE_VIEW (view));
  gtk_container_add (GTK_CONTAINER (scrolled), view);

  /* Catch Alt+Enter in the view */
  g_signal_connect (view, "key-press-event", G_CALLBACK (on_key_press), self);

  /* Menu bar ------------------------------------------------------ */
  GtkWidget *menu_bar      = gtk_menu_bar_new ();
  GtkWidget *file_menu     = gtk_menu_new ();
  GtkWidget *file_item     = gtk_menu_item_new_with_label ("File");
  GtkWidget *open_item     = gtk_menu_item_new_with_label ("Open…");
  GtkWidget *save_item     = gtk_menu_item_new_with_label ("Save");
  GtkWidget *saveas_item   = gtk_menu_item_new_with_label ("Save as…");
  GtkWidget *pref_item     = gtk_menu_item_new_with_label ("Preferences…");
  GtkWidget *quit_item     = gtk_menu_item_new_with_label ("Quit");

  gtk_menu_item_set_submenu (GTK_MENU_ITEM (file_item), file_menu);
  gtk_menu_shell_append (GTK_MENU_SHELL (file_menu), open_item);
  gtk_menu_shell_append (GTK_MENU_SHELL (file_menu), save_item);
  gtk_menu_shell_append (GTK_MENU_SHELL (file_menu), saveas_item);
  gtk_menu_shell_append (GTK_MENU_SHELL (file_menu), pref_item);
  gtk_menu_shell_append (GTK_MENU_SHELL (file_menu), quit_item);
  gtk_menu_shell_append (GTK_MENU_SHELL (menu_bar), file_item);

  g_signal_connect (open_item,   "activate", G_CALLBACK (file_open),   self);
  g_signal_connect (save_item,   "activate", G_CALLBACK (file_save),   self);
  g_signal_connect (saveas_item, "activate", G_CALLBACK (file_saveas), self);
  g_signal_connect (pref_item, "activate", G_CALLBACK (on_preferences), self);
  g_signal_connect (quit_item, "activate", G_CALLBACK (quit_menu_item), self);

  GtkWidget *interactions = GTK_WIDGET(interactions_view_new(self->swank));
  GtkWidget *paned = gtk_paned_new(GTK_ORIENTATION_VERTICAL);
  gtk_paned_pack1(GTK_PANED(paned), scrolled, TRUE, TRUE);
  gtk_paned_pack2(GTK_PANED(paned), interactions, FALSE, TRUE);

  GtkWidget *vbox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
  gtk_box_pack_start(GTK_BOX(vbox), menu_bar, FALSE, FALSE, 0);
  gtk_box_pack_start(GTK_BOX(vbox), paned, TRUE, TRUE, 0);
  gtk_container_add(GTK_CONTAINER(self->window), vbox);

  gtk_widget_show_all (self->window);
}

static void
app_startup (GApplication *app)
{
  g_debug("App.startup");
  /* Chain up first */
  G_APPLICATION_CLASS (app_parent_class)->startup (app);
}

static void
app_dispose (GObject *object)
{
  App *self = GLIDE_APP(object);

  g_debug("App.dispose");

  g_clear_pointer (&self->filename, g_free);
  g_clear_object (&self->preferences);
  g_clear_object (&self->swank);
  G_OBJECT_CLASS (app_parent_class)->dispose (object);
}

static void
app_class_init (AppClass *klass)
{
  g_debug("App.class_init");
  GApplicationClass *app_class = G_APPLICATION_CLASS (klass);
  GObjectClass      *obj_class = G_OBJECT_CLASS (klass);

  app_class->startup  = app_startup;
  app_class->activate = app_activate;
  obj_class->dispose  = app_dispose;
}

static void
app_init (App *self)
{
  g_debug("App.init");
  /* Everything that needs only the *instance* goes here */
  self->filename = NULL;
  self->preferences = NULL;
  self->swank = NULL;
}

STATIC App *
app_new (Preferences *prefs, SwankSession *swank)
{
  g_debug("App.new");
  g_return_val_if_fail (GLIDE_IS_SWANK_SESSION (swank), NULL);

  App *self = g_object_new (GLIDE_TYPE,
      /* GtkApplication properties */
      "application-id",    "org.example.Glide",
      "flags",             G_APPLICATION_HANDLES_OPEN,
      NULL);

  self->preferences = g_object_ref (prefs);
  self->swank       = g_object_ref (swank);
  return self;
}

STATIC GtkSourceBuffer *
app_get_source_buffer (App *self)
{
  g_debug("App.get_source_buffer");
  g_return_val_if_fail (GLIDE_IS_APP (self), NULL);
  return self->buffer;
}

STATIC const gchar *
app_get_filename (App *self)
{
  g_debug("App.get_filename");
  g_return_val_if_fail (GLIDE_IS_APP (self), NULL);
  return self->filename;
}

STATIC void
app_set_filename (App *self, const gchar *new_filename)
{
  g_debug("App.set_filename %s", new_filename ? new_filename : "(null)");
  g_return_if_fail (GLIDE_IS_APP (self));
  gchar *dup = new_filename ? g_strdup (new_filename) : NULL;
  g_free (self->filename);
  self->filename = dup;

  /* If you later register a “filename” property, notify here:
     g_object_notify_by_pspec (G_OBJECT (self), obj_props[PROP_FILENAME]); */
}

STATIC Preferences *
app_get_preferences (App *self)
{
  g_debug("App.get_preferences");
  g_return_val_if_fail (GLIDE_IS_APP (self), NULL);
  return self->preferences;
}

STATIC SwankSession *
app_get_swank (App *self)
{
  g_debug("App.get_swank");
  g_return_val_if_fail (GLIDE_IS_APP (self), NULL);
  return self->swank;
}

STATIC void
app_quit (App *self)
{
  g_debug("App.quit");
  g_return_if_fail (GLIDE_IS_APP (self));
  g_application_quit (G_APPLICATION (self));
}

STATIC void
app_on_quit (App *self)
{
  g_debug("App.on_quit");
  g_return_if_fail (GLIDE_IS_APP (self));
  /* TODO: check for unsaved changes, prompt the user, stop subprocesses, ... */
  app_quit (self);
}

STATIC gboolean
quit_delete_event (GtkWidget * /*widget*/, GdkEvent * /*event*/, gpointer data)
{
  g_debug("App.quit_delete_event");
  app_on_quit (GLIDE_APP (data));
  return TRUE;
}

STATIC void
quit_menu_item (GtkWidget * /*item*/, gpointer data)
{
  g_debug("App.quit_menu_item");
  app_on_quit (GLIDE_APP (data));
}
