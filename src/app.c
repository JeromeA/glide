#include "includes.h"
#include "app.h"
#include "file_open.h"
#include "file_new.h"
#include "project_new_wizard.h"
#include "preferences_dialog.h"
#include "evaluate.h"
#include "interactions_view.h"
#include "lisp_source_view.h"
#include "lisp_source_notebook.h"
#include "lisp_parser_view.h"
#include "project.h"
#include "status_bar.h"
#include "asdf_view.h"

/* Signal handlers */
STATIC gboolean quit_delete_event (GtkWidget * /*widget*/, GdkEvent * /*event*/, gpointer data);
STATIC void     quit_menu_item   (GtkWidget * /*item*/,   gpointer data);
STATIC void     on_notebook_paned_position(GObject *object, GParamSpec *pspec, gpointer data);

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
  StatusBar      *statusbar;
  StatusService  *status_service;
  GtkWidget      *notebook_paned;
  GtkWidget      *asdf_scrolled;
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
  self->notebook_paned = gtk_paned_new(GTK_ORIENTATION_HORIZONTAL);
  g_signal_connect(self->notebook_paned, "notify::position",
      G_CALLBACK(on_notebook_paned_position), self);
  gtk_paned_pack2(GTK_PANED(self->notebook_paned), notebook, TRUE, TRUE);

  LispSourceView *view = lisp_source_notebook_get_current_view(self->notebook);
  g_signal_connect (view, "key-press-event", G_CALLBACK (on_key_press), self);

  /* Menu bar ------------------------------------------------------ */
  GtkWidget *menu_bar      = gtk_menu_bar_new ();
  GtkWidget *file_menu     = gtk_menu_new();
  GtkWidget *file_item     = gtk_menu_item_new_with_label("File");

  GtkWidget *project_menu  = gtk_menu_new();
  GtkWidget *project_item  = gtk_menu_item_new_with_label("Project");
  GtkWidget *proj_new_item = gtk_menu_item_new_with_label("New…");
  GtkWidget *proj_open_item = gtk_menu_item_new_with_label("Open…");
  GtkWidget *proj_recent_item = gtk_menu_item_new_with_label("Recent");
  GtkWidget *recent_menu   = gtk_menu_new();

  GtkWidget *newfile_item  = gtk_menu_item_new_with_label("New file");
  GtkWidget *settings_item = gtk_menu_item_new_with_label("Settings…");
  GtkWidget *exit_item     = gtk_menu_item_new_with_label("Exit");

  gtk_menu_item_set_submenu(GTK_MENU_ITEM(file_item), file_menu);
  gtk_menu_item_set_submenu(GTK_MENU_ITEM(project_item), project_menu);
  gtk_menu_item_set_submenu(GTK_MENU_ITEM(proj_recent_item), recent_menu);
  gtk_menu_shell_append(GTK_MENU_SHELL(project_menu), proj_new_item);
  gtk_menu_shell_append(GTK_MENU_SHELL(project_menu), proj_open_item);
  gtk_menu_shell_append(GTK_MENU_SHELL(project_menu), proj_recent_item);
  gtk_menu_shell_append(GTK_MENU_SHELL(file_menu), project_item);
  gtk_menu_shell_append(GTK_MENU_SHELL(file_menu), newfile_item);
  gtk_menu_shell_append(GTK_MENU_SHELL(file_menu), gtk_separator_menu_item_new());
  gtk_menu_shell_append(GTK_MENU_SHELL(file_menu), settings_item);
  gtk_menu_shell_append(GTK_MENU_SHELL(file_menu), gtk_separator_menu_item_new());
  gtk_menu_shell_append(GTK_MENU_SHELL(file_menu), exit_item);
  gtk_menu_shell_append(GTK_MENU_SHELL(menu_bar), file_item);

  g_signal_connect(proj_new_item, "activate", G_CALLBACK(project_new_wizard), self);
  g_signal_connect(proj_open_item, "activate", G_CALLBACK(file_open), self);
  g_signal_connect(newfile_item, "activate", G_CALLBACK(file_new), self);
  g_signal_connect(settings_item, "activate", G_CALLBACK(on_preferences), self);
  g_signal_connect(exit_item, "activate", G_CALLBACK(quit_menu_item), self);

  GtkWidget *interactions = GTK_WIDGET(interactions_view_new(self->swank));
  GtkWidget *paned = gtk_paned_new(GTK_ORIENTATION_VERTICAL);
  gtk_paned_pack1(GTK_PANED(paned), self->notebook_paned, TRUE, TRUE);
  gtk_paned_pack2(GTK_PANED(paned), interactions, FALSE, TRUE);

  GtkWidget *vbox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
  gtk_box_pack_start(GTK_BOX(vbox), menu_bar, FALSE, FALSE, 0);
  gtk_box_pack_start(GTK_BOX(vbox), paned, TRUE, TRUE, 0);
  self->statusbar = status_bar_new(self->status_service);
  gtk_box_pack_start(GTK_BOX(vbox), GTK_WIDGET(self->statusbar), FALSE, FALSE, 0);
  gtk_container_add(GTK_CONTAINER(self->window), vbox);
  const gchar *proj = preferences_get_project_file(self->preferences);
  if (proj)
    file_open_path(self, proj);
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
  g_clear_object(&self->asdf_scrolled);
  g_clear_object(&self->notebook_paned);
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
  self->statusbar = NULL;
  self->status_service = NULL;
  self->notebook_paned = NULL;
  self->asdf_scrolled = NULL;
}

STATIC App *
app_new (Preferences *prefs, SwankSession *swank, Project *project, StatusService *status_service)
{
  g_debug("App.new");
  g_return_val_if_fail (swank, NULL);

  App *self = g_object_new (GLIDE_TYPE,
      /* GtkApplication properties */
      "application-id",    "org.example.Glide",
      "flags",             G_APPLICATION_HANDLES_OPEN,
      NULL);

  self->preferences    = preferences_ref(prefs);
  self->swank          = swank_session_ref(swank);
  self->project        = project_ref(project);
  self->status_service = status_service;
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

STATIC void
app_update_asdf_view(App *self)
{
  g_return_if_fail(GLIDE_IS_APP(self));
  if (!self->notebook_paned)
    return;
  if (self->asdf_scrolled) {
    gtk_widget_destroy(self->asdf_scrolled);
    self->asdf_scrolled = NULL;
  }
  Asdf *asdf = project_get_asdf(self->project);
  if (!asdf)
    return;
  GtkWidget *view = asdf_view_new(asdf);
  self->asdf_scrolled = gtk_scrolled_window_new(NULL, NULL);
  gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(self->asdf_scrolled),
      GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
  gtk_container_add(GTK_CONTAINER(self->asdf_scrolled), view);
  gtk_widget_show_all(self->asdf_scrolled);
  gtk_paned_pack1(GTK_PANED(self->notebook_paned), self->asdf_scrolled, FALSE, TRUE);
  gint width = preferences_get_asdf_view_width(self->preferences);
  gtk_paned_set_position(GTK_PANED(self->notebook_paned), width);
}

STATIC void
on_notebook_paned_position(GObject *object, GParamSpec * /*pspec*/, gpointer data)
{
  App *self = GLIDE_APP(data);
  if (!self->asdf_scrolled)
    return;
  Preferences *prefs = app_get_preferences(self);
  if (!prefs)
    return;
  gint pos = gtk_paned_get_position(GTK_PANED(object));
  preferences_set_asdf_view_width(prefs, pos);
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

STATIC StatusService *
app_get_status_service (App *self)
{
  g_debug("App.get_status_service");
  g_return_val_if_fail (GLIDE_IS_APP (self), NULL);
  return self->status_service;
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
