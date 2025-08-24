#include "includes.h"
#include "app.h"
#include "file_open.h"
#include "file_new.h"
#include "file_rename.h"
#include "file_save.h"
#include "project_new_wizard.h"
#include "preferences_dialog.h"
#include "evaluate.h"
#include "interactions_view.h"
#include "lisp_source_view.h"
#include "lisp_source_notebook.h"
#include "lisp_parser_view.h"
#include "project.h"
#include "project_file.h"
#include "status_bar.h"
#include "asdf_view.h"

/* Signal handlers */
STATIC gboolean quit_delete_event (GtkWidget * /*widget*/, GdkEvent * /*event*/, gpointer data);
STATIC void     quit_menu_item   (GtkWidget * /*item*/,   gpointer data);
STATIC void     close_project_menu_item(GtkWidget * /*item*/, gpointer data);
STATIC void     on_notebook_paned_position(GObject *object, GParamSpec *pspec, gpointer data);
STATIC void     app_update_recent_menu(App *self);
STATIC void     on_recent_project_activate(GtkWidget *item, gpointer data);
STATIC gboolean app_maybe_save_all(App *self);
STATIC gboolean app_close_project(App *self, gboolean forget_project);
STATIC void     on_asdf_view_selection_changed(GtkTreeSelection *selection, gpointer data);
STATIC void     on_notebook_switch_page(GtkNotebook *notebook, GtkWidget *page, guint page_num, gpointer data);
STATIC gboolean component_matches(const gchar *comp, const gchar *rel);

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
  GtkWidget      *recent_menu;
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
  if ((event->keyval == GDK_KEY_F6) &&
      (event->state & GDK_SHIFT_MASK))       /* Shift+F6 */
  {
    file_rename(NULL, self);
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
  g_signal_connect(self->notebook, "switch-page",
      G_CALLBACK(on_notebook_switch_page), self);

  LispSourceView *view = lisp_source_notebook_get_current_view(self->notebook);
  g_signal_connect (lisp_source_view_get_view(view), "key-press-event",
      G_CALLBACK (on_key_press), self);

  /* Menu bar ------------------------------------------------------ */
  GtkWidget *menu_bar      = gtk_menu_bar_new ();
  GtkWidget *file_menu     = gtk_menu_new();
  GtkWidget *file_item     = gtk_menu_item_new_with_label("File");

  GtkWidget *refactor_menu = gtk_menu_new();
  GtkWidget *refactor_item = gtk_menu_item_new_with_label("Refactor");
  GtkWidget *rename_item   = gtk_menu_item_new_with_label("Rename");

  GtkWidget *project_menu  = gtk_menu_new();
  GtkWidget *project_item  = gtk_menu_item_new_with_label("Project");
  GtkWidget *proj_new_item = gtk_menu_item_new_with_label("New…");
  GtkWidget *proj_open_item = gtk_menu_item_new_with_label("Open…");
  GtkWidget *proj_recent_item = gtk_menu_item_new_with_label("Recent");
  GtkWidget *recent_menu   = gtk_menu_new();
  self->recent_menu = recent_menu;

  GtkWidget *newfile_item  = gtk_menu_item_new_with_label("New file");
  GtkWidget *saveall_item  = gtk_menu_item_new_with_label("Save all");
  GtkWidget *closeproj_item = gtk_menu_item_new_with_label("Close project");
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
  gtk_menu_shell_append(GTK_MENU_SHELL(file_menu), saveall_item);
  gtk_menu_shell_append(GTK_MENU_SHELL(file_menu), closeproj_item);
  gtk_menu_shell_append(GTK_MENU_SHELL(file_menu), gtk_separator_menu_item_new());
  gtk_menu_shell_append(GTK_MENU_SHELL(file_menu), settings_item);
  gtk_menu_shell_append(GTK_MENU_SHELL(file_menu), gtk_separator_menu_item_new());
  gtk_menu_shell_append(GTK_MENU_SHELL(file_menu), exit_item);
  gtk_menu_shell_append(GTK_MENU_SHELL(menu_bar), file_item);
  gtk_menu_item_set_submenu(GTK_MENU_ITEM(refactor_item), refactor_menu);
  gtk_menu_shell_append(GTK_MENU_SHELL(refactor_menu), rename_item);
  gtk_menu_shell_append(GTK_MENU_SHELL(menu_bar), refactor_item);

  g_signal_connect(proj_new_item, "activate", G_CALLBACK(project_new_wizard), self);
  g_signal_connect(proj_open_item, "activate", G_CALLBACK(file_open), self);
  g_signal_connect(newfile_item, "activate", G_CALLBACK(file_new), self);
  g_signal_connect(saveall_item, "activate", G_CALLBACK(file_save_all), self);
  g_signal_connect(closeproj_item, "activate", G_CALLBACK(close_project_menu_item), self);
  g_signal_connect(settings_item, "activate", G_CALLBACK(on_preferences), self);
  g_signal_connect(exit_item, "activate", G_CALLBACK(quit_menu_item), self);
  g_signal_connect(rename_item, "activate", G_CALLBACK(file_rename), self);

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
  if (proj) {
    if (file_open_path(self, proj)) {
      const gchar *last = preferences_get_last_file(self->preferences);
      gint pos = preferences_get_cursor_position(self->preferences);
      if (last) {
        Project *project = app_get_project(self);
        guint count = project_get_file_count(project);
        for (guint i = 0; i < count; i++) {
          ProjectFile *pf = project_get_file(project, i);
          if (g_strcmp0(project_file_get_path(pf), last) == 0) {
            gtk_notebook_set_current_page(GTK_NOTEBOOK(self->notebook), i);
            LispSourceView *view = lisp_source_notebook_get_current_view(self->notebook);
            if (view) {
              GtkTextBuffer *buffer = GTK_TEXT_BUFFER(lisp_source_view_get_buffer(view));
              GtkTextIter iter;
              gtk_text_buffer_get_iter_at_offset(buffer, &iter, pos);
              gtk_text_buffer_place_cursor(buffer, &iter);
              gtk_text_view_scroll_to_iter(GTK_TEXT_VIEW(view), &iter, 0.0, FALSE, 0, 0);
              app_connect_view(self, view);
            }
            break;
          }
        }
      }
    }
  }
  app_update_recent_menu(self);
  gtk_widget_show_all(self->window);
  LispSourceView *current_view = lisp_source_notebook_get_current_view(self->notebook);
  if (current_view)
    gtk_widget_grab_focus(GTK_WIDGET(current_view));
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
  g_clear_object(&self->recent_menu);
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
  self->recent_menu = NULL;
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
  g_signal_connect(lisp_source_view_get_view(view), "key-press-event",
      G_CALLBACK(on_key_press), self);
}

STATIC ProjectFile *
app_get_current_file(App *self)
{
  g_return_val_if_fail(GLIDE_IS_APP(self), NULL);
  LispSourceView *view = app_get_source_view(self);
  return view ? lisp_source_view_get_file(view) : NULL;
}

STATIC gboolean
component_matches(const gchar *comp, const gchar *rel)
{
  if (!comp || !rel)
    return FALSE;
  if (g_strcmp0(comp, rel) == 0)
    return TRUE;
  gchar *base = g_path_get_basename(rel);
  gchar *dot = g_strrstr(base, ".");
  if (dot)
    *dot = '\0';
  gboolean match = g_strcmp0(comp, base) == 0;
  g_free(base);
  return match;
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
  GtkTreeSelection *sel = gtk_tree_view_get_selection(GTK_TREE_VIEW(view));
  g_signal_connect(sel, "changed", G_CALLBACK(on_asdf_view_selection_changed), self);
  self->asdf_scrolled = gtk_scrolled_window_new(NULL, NULL);
  gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(self->asdf_scrolled),
      GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
  gtk_container_add(GTK_CONTAINER(self->asdf_scrolled), view);
  gtk_widget_show_all(self->asdf_scrolled);
  gtk_paned_pack1(GTK_PANED(self->notebook_paned), self->asdf_scrolled, FALSE, TRUE);
  gint width = preferences_get_asdf_view_width(self->preferences);
  gtk_paned_set_position(GTK_PANED(self->notebook_paned), width);
  on_notebook_switch_page(GTK_NOTEBOOK(self->notebook), NULL,
      gtk_notebook_get_current_page(GTK_NOTEBOOK(self->notebook)), self);
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

STATIC void
on_asdf_view_selection_changed(GtkTreeSelection *selection, gpointer data)
{
  App *self = GLIDE_APP(data);
  GtkTreeModel *model;
  GtkTreeIter iter;
  if (!gtk_tree_selection_get_selected(selection, &model, &iter))
    return;
  GtkTreeIter parent;
  if (!gtk_tree_model_iter_parent(model, &parent, &iter))
    return;
  gchar *parent_text = NULL;
  gtk_tree_model_get(model, &parent, 0, &parent_text, -1);
  gboolean is_component = g_strcmp0(parent_text, "components") == 0;
  g_free(parent_text);
  if (!is_component)
    return;
  gchar *comp = NULL;
  gtk_tree_model_get(model, &iter, 0, &comp, -1);
  if (!comp)
    return;
  guint count = project_get_file_count(self->project);
  for (guint i = 0; i < count; i++) {
    ProjectFile *file = project_get_file(self->project, i);
    const gchar *rel = project_file_get_relative_path(file);
    if (component_matches(comp, rel)) {
      gtk_notebook_set_current_page(GTK_NOTEBOOK(self->notebook), i);
      break;
    }
  }
  g_free(comp);
}

STATIC void
on_notebook_switch_page(GtkNotebook * /*notebook*/, GtkWidget *page, guint /*page_num*/, gpointer data)
{
  App *self = GLIDE_APP(data);
  if (!self->asdf_scrolled)
    return;
  GtkWidget *view = gtk_bin_get_child(GTK_BIN(self->asdf_scrolled));
  if (!view)
    return;
  if (!page)
    return;
  ProjectFile *file = lisp_source_view_get_file(LISP_SOURCE_VIEW(page));
  if (!file)
    return;
  const gchar *rel = project_file_get_relative_path(file);
  if (!rel)
    return;
  asdf_view_select_file(ASDF_VIEW(view), rel);
}

STATIC void
app_update_recent_menu(App *self)
{
  g_return_if_fail(GLIDE_IS_APP(self));
  if (!self->recent_menu)
    return;
  GList *children = gtk_container_get_children(GTK_CONTAINER(self->recent_menu));
  for (GList *l = children; l; l = l->next)
    gtk_widget_destroy(GTK_WIDGET(l->data));
  g_list_free(children);
  const GList *recent = preferences_get_recent_projects(self->preferences);
  for (const GList *l = recent; l; l = l->next) {
    const gchar *path = l->data;
    gchar *label = g_path_get_basename(path);
    GtkWidget *item = gtk_menu_item_new_with_label(label);
    g_free(label);
    g_object_set_data_full(G_OBJECT(item), "project-path", g_strdup(path), g_free);
    g_signal_connect(item, "activate", G_CALLBACK(on_recent_project_activate), self);
    gtk_menu_shell_append(GTK_MENU_SHELL(self->recent_menu), item);
  }
  gtk_widget_show_all(self->recent_menu);
}

STATIC void
on_recent_project_activate(GtkWidget *item, gpointer data)
{
  App *self = GLIDE_APP(data);
  const gchar *path = g_object_get_data(G_OBJECT(item), "project-path");
  if (path)
    file_open_path(self, path);
}


STATIC Preferences *
app_get_preferences (App *self)
{
  g_return_val_if_fail (GLIDE_IS_APP (self), NULL);
  return self->preferences;
}

STATIC SwankSession *
app_get_swank (App *self)
{
  g_return_val_if_fail (GLIDE_IS_APP (self), NULL);
  return self->swank;
}

STATIC StatusService *
app_get_status_service (App *self)
{
  g_return_val_if_fail (GLIDE_IS_APP (self), NULL);
  return self->status_service;
}

STATIC gboolean
app_maybe_save_all(App *self)
{
  LispSourceNotebook *notebook = app_get_notebook(self);
  if (!notebook)
    return TRUE;
  gint pages = gtk_notebook_get_n_pages(GTK_NOTEBOOK(notebook));
  gboolean modified = FALSE;
  for (gint i = 0; i < pages; i++) {
    GtkWidget *view = gtk_notebook_get_nth_page(GTK_NOTEBOOK(notebook), i);
    GtkTextBuffer *buffer = view ? GTK_TEXT_BUFFER(lisp_source_view_get_buffer(LISP_SOURCE_VIEW(view))) : NULL;
    if (buffer && gtk_text_buffer_get_modified(buffer)) {
      modified = TRUE;
      break;
    }
  }
  if (!modified)
    return TRUE;
  GtkWidget *dialog = gtk_message_dialog_new(NULL, GTK_DIALOG_MODAL,
      GTK_MESSAGE_QUESTION, GTK_BUTTONS_NONE,
      "Save changes to project?");
  gtk_dialog_add_button(GTK_DIALOG(dialog), "_Cancel", GTK_RESPONSE_CANCEL);
  gtk_dialog_add_button(GTK_DIALOG(dialog), "_Discard", GTK_RESPONSE_REJECT);
  gtk_dialog_add_button(GTK_DIALOG(dialog), "_Save", GTK_RESPONSE_ACCEPT);
  gint res = gtk_dialog_run(GTK_DIALOG(dialog));
  gtk_widget_destroy(dialog);
  if (res == GTK_RESPONSE_CANCEL)
    return FALSE;
  if (res == GTK_RESPONSE_ACCEPT)
    file_save_all(NULL, self);
  return TRUE;
}

STATIC gboolean
app_close_project(App *self, gboolean forget_project)
{
  g_debug("App.close_project forget=%d", forget_project);
  g_return_val_if_fail(GLIDE_IS_APP(self), FALSE);
  if (!app_maybe_save_all(self))
    return FALSE;
  Project *project = app_get_project(self);
  project_clear(project);
  Preferences *prefs = app_get_preferences(self);
  if (prefs && forget_project) {
    preferences_set_project_file(prefs, NULL);
    preferences_set_last_file(prefs, NULL);
    preferences_set_cursor_position(prefs, 0);
  }
  app_update_asdf_view(self);
  return TRUE;
}

STATIC void
close_project_menu_item(GtkWidget * /*item*/, gpointer data)
{
  g_debug("App.close_project_menu_item");
  app_close_project(GLIDE_APP(data), TRUE);
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
  Preferences *prefs = app_get_preferences(self);
  ProjectFile *pf = app_get_current_file(self);
  if (prefs && pf) {
    const gchar *path = project_file_get_path(pf);
    preferences_set_last_file(prefs, path);
    GtkTextBuffer *buffer = project_file_get_buffer(pf);
    if (buffer) {
      GtkTextMark *insert_mark = gtk_text_buffer_get_insert(buffer);
      GtkTextIter iter;
      gtk_text_buffer_get_iter_at_mark(buffer, &iter, insert_mark);
      gint pos = gtk_text_iter_get_offset(&iter);
      preferences_set_cursor_position(prefs, pos);
    }
  }
  if (!app_close_project(self, FALSE))
    return;
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
