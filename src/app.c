#include "app.h"
#include "project_open.h"
#include "interactions_view.h"
#include "editor.h"
#include "editor_container.h"
#include "editor_manager.h"
#include "project.h"
#include "document.h"
#include "status_bar.h"
#include "project_view.h"
#include "menu_bar.h"
#include "actions.h"
#include "util.h"
#include <gtk/gtk.h>
#include <gtksourceview/gtksource.h>

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
  EditorContainer *editor_container;
  EditorManager *editor_manager;
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
  GtkWidget *editor_container = editor_container_new();
  self->editor_container = EDITOR_CONTAINER(editor_container);
  g_clear_object(&self->editor_manager);
  self->editor_manager = editor_manager_new(self->project, self->editor_container);
  self->notebook_paned = gtk_paned_new(GTK_ORIENTATION_HORIZONTAL);
  g_signal_connect(self->notebook_paned, "notify::position",
      G_CALLBACK(on_notebook_paned_position), self);
  self->project_scrolled = gtk_scrolled_window_new(NULL, NULL);
  gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(self->project_scrolled),
      GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
  gtk_paned_pack1(GTK_PANED(self->notebook_paned), self->project_scrolled, FALSE, TRUE);
  gtk_paned_pack2(GTK_PANED(self->notebook_paned), editor_container, TRUE, TRUE);
  gint width = preferences_get_project_view_width(self->preferences);
  gtk_paned_set_position(GTK_PANED(self->notebook_paned), width);
  g_signal_connect(self->editor_container, "switch-page",
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
  if (proj)
    project_open_path(self, proj);
  app_update_project_view(self);
  app_update_recent_menu(self);
  gtk_widget_show_all(self->window);
  Editor *current_view = editor_container_get_current_editor(self->editor_container);
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

  // Load tooltip CSS
  GtkCssProvider *provider = gtk_css_provider_new();
  gtk_css_provider_load_from_data(provider,
      // "tooltip * { color: #000; background-color: #eee; text-shadow: none; }",
      "tooltip label { color: #000; background-color: #eee; text-shadow: none; border: none } "
      "tooltip { background-color: #eee; border: 1px solid #aaa; border-radius: 0; box-shadow: 0 0 20px -20px #aaa; }",
      -1, NULL);
  gtk_style_context_add_provider_for_screen(
      gdk_screen_get_default(),
      GTK_STYLE_PROVIDER(provider),
      GTK_STYLE_PROVIDER_PRIORITY_APPLICATION);
  g_object_unref(provider);
}

static void
app_dispose (GObject *object)
{
  App *self = GLIDE_APP(object);

  LOG(1, "App.dispose");

  g_clear_object(&self->editor_manager);
  if (self->project) {
    project_unref(self->project);
    self->project = NULL;
  }
  g_clear_object(&self->editor_container);
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
  self->editor_container = NULL;
  self->editor_manager = NULL;
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
  if (!self->editor_container)
    return NULL;
  return editor_container_get_current_editor(self->editor_container);
}

STATIC EditorContainer *
app_get_editor_container(App *self)
{
  g_return_val_if_fail(GLIDE_IS_APP(self), NULL);
  return self->editor_container;
}

STATIC EditorManager *
app_get_editor_manager(App *self)
{
  g_return_val_if_fail(GLIDE_IS_APP(self), NULL);
  return self->editor_manager;
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

STATIC Document *
app_get_current_document(App *self)
{
  g_return_val_if_fail(GLIDE_IS_APP(self), NULL);
  Editor *view = app_get_editor(self);
  return view ? editor_get_document(view) : NULL;
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
  on_notebook_switch_page(GTK_NOTEBOOK(self->editor_container), NULL,
      gtk_notebook_get_current_page(GTK_NOTEBOOK(self->editor_container)), self);
}

STATIC void
app_restore_last_file(App *self)
{
  g_return_if_fail(GLIDE_IS_APP(self));
  if (!self->editor_container)
    return;
  Preferences *prefs = app_get_preferences(self);
  if (!prefs)
    return;
  const gchar *last = preferences_get_last_file(prefs);
  if (!last)
    return;
  Project *project = app_get_project(self);
  if (!project)
    return;
  guint count = project_get_document_count(project);
  for (guint i = 0; i < count; i++) {
    Document *document = project_get_document(project, i);
    if (g_strcmp0(document_get_path(document), last) == 0) {
      gtk_notebook_set_current_page(GTK_NOTEBOOK(self->editor_container), i);
      Editor *view = editor_container_get_current_editor(self->editor_container);
      if (!view)
        return;
      GtkWidget *text_view = editor_get_view(view);
      GtkTextBuffer *buffer = GTK_TEXT_BUFFER(editor_get_buffer(view));
      gint pos = preferences_get_cursor_position(prefs);
      GtkTextIter iter;
      gtk_text_buffer_get_iter_at_offset(buffer, &iter, pos);
      gtk_text_buffer_place_cursor(buffer, &iter);
      gtk_text_view_scroll_to_iter(GTK_TEXT_VIEW(text_view), &iter,
          0.0, FALSE, 0, 0);
      return;
    }
  }
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

  Document *document = NULL;
  gtk_tree_model_get(model, &iter,
      PROJECT_VIEW_COL_OBJECT, &document,
      -1);
  if (!document)
    return;
  guint count = project_get_document_count(self->project);
  for (guint i = 0; i < count; i++) {
    if (project_get_document(self->project, i) == document) {
      gtk_notebook_set_current_page(GTK_NOTEBOOK(self->editor_container), i);
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
  Document *document = editor_get_document(GLIDE_EDITOR(page));
  if (!document)
    return;
  const gchar *rel = document_get_relative_path(document);
  if (rel)
    project_view_select_file(PROJECT_VIEW(view), rel);

  Preferences *prefs = app_get_preferences(self);
  if (prefs) {
    preferences_set_last_file(prefs, document_get_path(document));
    EditorManager *manager = app_get_editor_manager(self);
    GtkTextBuffer *buffer = manager ? editor_manager_get_buffer(manager, document) : NULL;
    g_return_if_fail(buffer);
    GtkTextMark *mark = gtk_text_buffer_get_insert(buffer);
    GtkTextIter iter;
    gtk_text_buffer_get_iter_at_mark(buffer, &iter, mark);
    gint pos = gtk_text_iter_get_offset(&iter);
    preferences_set_cursor_position(prefs, pos);
  }
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
  Document *document = app_get_current_document(self);
  if (prefs && document) {
    const gchar *path = document_get_path(document);
    preferences_set_last_file(prefs, path);
    EditorManager *manager = app_get_editor_manager(self);
    GtkTextBuffer *buffer = manager ? editor_manager_get_buffer(manager, document) : NULL;
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

