#include "includes.h"
#include "app.h"
#include "file_open.h"
#include "file_save.h"
#include "preferences_dialog.h"
#include "evaluate.h"
#include "interactions_view.h"
#include "lisp_source_view.h"
#include "lisp_source_notebook.h"
#include "lisp_parser_view.h"
#include "project.h"

/* Signal handlers */
STATIC gboolean quit_delete_event (GtkWidget * /*widget*/, GdkEvent * /*event*/, gpointer data);
STATIC void     quit_menu_item   (GtkWidget * /*item*/,   gpointer data);

/* === Instance structure ================================================= */
struct _App
{
  GtkApplication  parent_instance;

  /* UI pointers we want to reuse */
  GtkWidget      *window;
  LispSourceNotebook *notebook;
  Preferences    *preferences;
  SwankSession   *swank;
  Project        *project;
};

static void
on_show_parser(App *self)
{
  GtkWidget *win = gtk_window_new(GTK_WINDOW_TOPLEVEL);
  LispSourceView *current = lisp_source_notebook_get_current_view(self->notebook);
  ProjectFile *file = lisp_source_view_get_file(current);
  GtkWidget *view = lisp_parser_view_new(file);
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

  /* Source views notebook */
  GtkWidget *notebook = lisp_source_notebook_new (self->project);
  self->notebook = LISP_SOURCE_NOTEBOOK(notebook);

  LispSourceView *view = lisp_source_notebook_get_current_view(self->notebook);
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
  gtk_paned_pack1(GTK_PANED(paned), notebook, TRUE, TRUE);
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

  if (self->project) {
    project_unref(self->project);
    self->project = NULL;
  }
  g_clear_object(&self->notebook);
  if (self->preferences) {
    preferences_unref(self->preferences);
    self->preferences = NULL;
  }
  g_clear_pointer(&self->swank, swank_session_unref);
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
  self->preferences = NULL;
  self->swank = NULL;
  self->project = NULL;
  self->notebook = NULL;
}

STATIC App *
app_new (Preferences *prefs, SwankSession *swank, Project *project)
{
  g_debug("App.new");
  g_return_val_if_fail (swank, NULL);

  App *self = g_object_new (GLIDE_TYPE,
      /* GtkApplication properties */
      "application-id",    "org.example.Glide",
      "flags",             G_APPLICATION_HANDLES_OPEN,
      NULL);

  self->preferences = preferences_ref(prefs);
  self->swank       = swank_session_ref(swank);
  self->project     = project_ref(project);
  return self;
}


STATIC LispSourceView *
app_get_source_view(App *self)
{
  g_debug("App.get_source_view");
  g_return_val_if_fail (GLIDE_IS_APP (self), NULL);
  if (!self->notebook)
    return NULL;
  return lisp_source_notebook_get_current_view(self->notebook);
}

STATIC LispSourceNotebook *
app_get_notebook(App *self)
{
  g_return_val_if_fail(GLIDE_IS_APP(self), NULL);
  return self->notebook;
}

STATIC Project *
app_get_project(App *self)
{
  g_return_val_if_fail(GLIDE_IS_APP(self), NULL);
  return self->project;
}

STATIC void
app_connect_view(App *self, LispSourceView *view)
{
  g_return_if_fail(GLIDE_IS_APP(self));
  g_return_if_fail(LISP_IS_SOURCE_VIEW(view));
  g_signal_connect(view, "key-press-event", G_CALLBACK(on_key_press), self);
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
