#include "includes.h"
#include "app.h"
#include "file_open.h"
#include "interactions_view.h"
#include "editor.h"
#include "lisp_source_notebook.h"
#include "project.h"
#include "project_file.h"
#include "status_bar.h"
#include "project_view.h"
#include "menu_bar.h"
#include "actions.h"
#include "util.h"

/* Signal handlers */
STATIC void     on_notebook_paned_position(GObject *object, GParamSpec *pspec, gpointer data);
STATIC void     app_update_recent_menu(App *self);
STATIC void     on_project_view_selection_changed(GtkTreeSelection *selection, gpointer data);
STATIC void     on_notebook_switch_page(GtkNotebook *notebook, GtkWidget *page, guint page_num, gpointer data);
STATIC void     on_window_size_allocate(GtkWidget *widget, GtkAllocation * /*allocation*/, gpointer data);

/* === Instance structure ================================================= */
struct _App
{
  GtkApplication  parent_instance;

  /* UI pointers we want to reuse */
  GtkWidget      *window;
  LispSourceNotebook *notebook;
  Preferences    *preferences;
  ReplSession   *glide;
  Project        *project;
  StatusBar      *statusbar;
  StatusService  *status_service;
  GtkWidget      *notebook_paned;
  GtkWidget      *project_scrolled;
  GMenu          *recent_menu;
};

STATIC void
on_window_size_allocate(GtkWidget *widget, GtkAllocation * /*allocation*/, gpointer data)
{
  App *self = (App *) data;
  gint width;
  gint height;
  gtk_window_get_size(GTK_WINDOW(widget), &width, &height);
  preferences_set_window_width(self->preferences, width);
  preferences_set_window_height(self->preferences, height);
}


/* === GObject boiler-plate ============================================== */
G_DEFINE_TYPE(App, app, GTK_TYPE_APPLICATION)

/* ---  class_init ------------------------------------------------------- */
static void
app_activate (GApplication *app)
{
  App *self = GLIDE_APP(app);

  LOG(1, "App.activate");

  /*--------------------------------------------------------------*
   *  Build the UI (this is almost a verbatim move from app.c)    *
   *--------------------------------------------------------------*/
  self->window = gtk_application_window_new(GTK_APPLICATION(app));
  gtk_window_set_title(GTK_WINDOW(self->window), "Glide");
  gtk_window_set_default_size(GTK_WINDOW(self->window),
      preferences_get_window_width(self->preferences),
      preferences_get_window_height(self->preferences));
  g_signal_connect(self->window, "delete-event",
      G_CALLBACK(on_quit_delete_event), self);
  g_signal_connect(self->window, "size-allocate",
      G_CALLBACK(on_window_size_allocate), self);

  /* Source views notebook */
  GtkWidget *notebook = lisp_source_notebook_new (self->project);
  self->notebook = LISP_SOURCE_NOTEBOOK(notebook);
  self->notebook_paned = gtk_paned_new(GTK_ORIENTATION_HORIZONTAL);
  g_signal_connect(self->notebook_paned, "notify::position",
      G_CALLBACK(on_notebook_paned_position), self);
  self->project_scrolled = gtk_scrolled_window_new(NULL, NULL);
  gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(self->project_scrolled),
      GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
  gtk_paned_pack1(GTK_PANED(self->notebook_paned), self->project_scrolled, FALSE, TRUE);
  gtk_paned_pack2(GTK_PANED(self->notebook_paned), notebook, TRUE, TRUE);
  gint width = preferences_get_project_view_width(self->preferences);
  gtk_paned_set_position(GTK_PANED(self->notebook_paned), width);
  g_signal_connect(self->notebook, "switch-page",
      G_CALLBACK(on_notebook_switch_page), self);

  GtkWidget *menu_bar = menu_bar_new(self);

  GtkWidget *interactions = GTK_WIDGET(interactions_view_new(self->glide));
  gtk_widget_set_vexpand(interactions, TRUE);
  GtkWidget *paned = gtk_paned_new(GTK_ORIENTATION_VERTICAL);
  gtk_paned_pack1(GTK_PANED(paned), self->notebook_paned, TRUE, TRUE);
  gtk_paned_pack2(GTK_PANED(paned), interactions, TRUE, TRUE);

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
            Editor *view = lisp_source_notebook_get_current_editor(self->notebook);
            if (view) {
              GtkWidget *text_view = editor_get_view(view);
              GtkTextBuffer *buffer =
                  GTK_TEXT_BUFFER(editor_get_buffer(view));
              GtkTextIter iter;
              gtk_text_buffer_get_iter_at_offset(buffer, &iter, pos);
              gtk_text_buffer_place_cursor(buffer, &iter);
              gtk_text_view_scroll_to_iter(GTK_TEXT_VIEW(text_view), &iter,
                  0.0, FALSE, 0, 0);
                app_connect_editor(self, view);
            }
            break;
          }
        }
      }
    }
  }
  app_update_project_view(self);
  app_update_recent_menu(self);
  gtk_widget_show_all(self->window);
  Editor *current_view = lisp_source_notebook_get_current_editor(self->notebook);
  if (current_view)
    gtk_widget_grab_focus(editor_get_view(current_view));
}

static void
app_startup (GApplication *app)
{
  LOG(1, "App.startup");
  /* Chain up first */
  G_APPLICATION_CLASS (app_parent_class)->startup (app);
  g_return_if_fail(glide_is_ui_thread());
  App *self = GLIDE_APP(app);
  self->project = project_new(self->glide);
  actions_init(self);
}

static void
app_dispose (GObject *object)
{
  App *self = GLIDE_APP(object);

  LOG(1, "App.dispose");

  if (self->project) {
    project_unref(self->project);
    self->project = NULL;
  }
  g_clear_object(&self->notebook);
  g_clear_object(&self->project_scrolled);
  g_clear_object(&self->notebook_paned);
  g_clear_object(&self->recent_menu);
  if (self->preferences) {
    preferences_unref(self->preferences);
    self->preferences = NULL;
  }
  g_clear_pointer(&self->glide, repl_session_unref);
  G_OBJECT_CLASS (app_parent_class)->dispose (object);
}

static void
app_class_init (AppClass *klass)
{
  LOG(1, "App.class_init");
  GApplicationClass *app_class = G_APPLICATION_CLASS (klass);
  GObjectClass      *obj_class = G_OBJECT_CLASS (klass);

  app_class->startup  = app_startup;
  app_class->activate = app_activate;
  obj_class->dispose  = app_dispose;
}

static void
app_init (App *self)
{
  LOG(1, "App.init");
  /* Everything that needs only the *instance* goes here */
  self->preferences = NULL;
  self->glide = NULL;
  self->project = NULL;
  self->notebook = NULL;
  self->statusbar = NULL;
  self->status_service = NULL;
  self->notebook_paned = NULL;
  self->project_scrolled = NULL;
  self->recent_menu = NULL;
}

STATIC App *
app_new (Preferences *prefs, ReplSession *glide, StatusService *status_service)
{
  LOG(1, "App.new");
  g_return_val_if_fail (glide, NULL);

  App *self = g_object_new (GLIDE_TYPE,
      /* GtkApplication properties */
      "application-id",    "org.example.Glide",
      "flags",             G_APPLICATION_HANDLES_OPEN,
      NULL);

  self->preferences    = preferences_ref(prefs);
  self->glide          = repl_session_ref(glide);
  self->status_service = status_service;
  return self;
}


STATIC Editor *
app_get_editor(App *self)
{
  LOG(1, "App.get_editor");
  g_return_val_if_fail (GLIDE_IS_APP (self), NULL);
  if (!self->notebook)
    return NULL;
  return lisp_source_notebook_get_current_editor(self->notebook);
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
app_connect_editor(App *self, Editor *editor)
{
  g_return_if_fail(GLIDE_IS_APP(self));
  g_return_if_fail(GLIDE_IS_EDITOR(editor));
}

STATIC ProjectFile *
app_get_current_file(App *self)
{
  g_return_val_if_fail(GLIDE_IS_APP(self), NULL);
  Editor *view = app_get_editor(self);
  return view ? editor_get_file(view) : NULL;
}


STATIC void
app_update_project_view(App *self)
{
  g_return_if_fail(GLIDE_IS_APP(self));
  if (!self->project_scrolled)
    return;
  GtkWidget *old = gtk_bin_get_child(GTK_BIN(self->project_scrolled));
  if (old)
    gtk_widget_destroy(old);
  Asdf *asdf = project_get_asdf(self->project);
  GtkWidget *view = project_view_new(asdf, self);
  GtkTreeSelection *sel = gtk_tree_view_get_selection(GTK_TREE_VIEW(view));
  g_signal_connect(sel, "changed", G_CALLBACK(on_project_view_selection_changed), self);
  gtk_container_add(GTK_CONTAINER(self->project_scrolled), view);
  gtk_widget_show_all(self->project_scrolled);
  on_notebook_switch_page(GTK_NOTEBOOK(self->notebook), NULL,
      gtk_notebook_get_current_page(GTK_NOTEBOOK(self->notebook)), self);
}

STATIC void
on_notebook_paned_position(GObject *object, GParamSpec * /*pspec*/, gpointer data)
{
  App *self = GLIDE_APP(data);
  Preferences *prefs = app_get_preferences(self);
  if (!prefs)
    return;
  gint pos = gtk_paned_get_position(GTK_PANED(object));
  preferences_set_project_view_width(prefs, pos);
}

STATIC void
on_project_view_selection_changed(GtkTreeSelection *selection, gpointer data)
{
  App *self = GLIDE_APP(data);
  GtkTreeModel *model;
  GtkTreeIter iter;
  if (!gtk_tree_selection_get_selected(selection, &model, &iter))
    return;

  gint kind = 0;
  gtk_tree_model_get(model, &iter,
      PROJECT_VIEW_COL_KIND, &kind,
      -1);
  if (kind != PROJECT_VIEW_KIND_SRC)
    return;

  ProjectFile *file = NULL;
  gtk_tree_model_get(model, &iter,
      PROJECT_VIEW_COL_OBJECT, &file,
      -1);
  if (!file)
    return;
  guint count = project_get_file_count(self->project);
  for (guint i = 0; i < count; i++) {
    if (project_get_file(self->project, i) == file) {
      gtk_notebook_set_current_page(GTK_NOTEBOOK(self->notebook), i);
      break;
    }
  }
}

STATIC void
on_notebook_switch_page(GtkNotebook * /*notebook*/, GtkWidget *page, guint /*page_num*/, gpointer data)
{
  App *self = GLIDE_APP(data);
  GtkWidget *view = gtk_bin_get_child(GTK_BIN(self->project_scrolled));
  if (!view)
    return;
  if (!page)
    return;
  ProjectFile *file = editor_get_file(GLIDE_EDITOR(page));
  if (!file)
    return;
  const gchar *rel = project_file_get_relative_path(file);
  if (!rel)
    return;
  project_view_select_file(PROJECT_VIEW(view), rel);
}

STATIC void
app_update_recent_menu(App *self)
{
  g_return_if_fail(GLIDE_IS_APP(self));
  if (!self->recent_menu)
    return;
  gint n = g_menu_model_get_n_items(G_MENU_MODEL(self->recent_menu));
  for (gint i = n - 1; i >= 0; i--)
    g_menu_remove(self->recent_menu, i);
  const GList *recent = preferences_get_recent_projects(self->preferences);
  for (const GList *l = recent; l; l = l->next) {
    const gchar *path = l->data;
    gchar *label = g_path_get_basename(path);
    GMenuItem *item = g_menu_item_new(label, NULL);
    g_free(label);
    g_menu_item_set_action_and_target_value(item, "app.recent-project",
        g_variant_new_string(path));
    g_menu_append_item(self->recent_menu, item);
    g_object_unref(item);
  }
}



STATIC Preferences *
app_get_preferences (App *self)
{
  g_return_val_if_fail (GLIDE_IS_APP (self), NULL);
  return self->preferences;
}

STATIC ReplSession *
app_get_glide (App *self)
{
  g_return_val_if_fail (GLIDE_IS_APP (self), NULL);
  return self->glide;
}

STATIC StatusService *
app_get_status_service (App *self)
{
  g_return_val_if_fail (GLIDE_IS_APP (self), NULL);
  return self->status_service;
}

void
app_set_recent_menu(App *self, GMenu *menu)
{
  g_return_if_fail(GLIDE_IS_APP(self));
  self->recent_menu = menu;
}


STATIC void
app_quit (App *self)
{
  LOG(1, "App.quit");
  g_return_if_fail (GLIDE_IS_APP (self));
  g_application_quit (G_APPLICATION (self));
}

STATIC void
app_on_quit (App *self)
{
  LOG(1, "App.on_quit");
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

