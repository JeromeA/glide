#include "project_view.h"
#include "project_file.h"
#include "app.h"
#include "file_new.h"
#include "file_add.h"
#include "file_delete.h"
#include "file_rename.h"

struct _ProjectView {
  GtkTreeView parent_instance;
  Asdf *asdf;
  Project *project;
  App *app;
  GtkTreeStore *store;
};

G_DEFINE_TYPE(ProjectView, project_view, GTK_TYPE_TREE_VIEW)

static void project_view_populate_store(ProjectView *self);
static gboolean filename_matches(const gchar *component, const gchar *file);
static gchar *get_selected_component(ProjectView *self);
static gboolean on_button_press(GtkWidget *widget, GdkEventButton *event, gpointer data);
static gboolean dispatch_package_added(gpointer data);
static void on_package_added(Project *project, Package *package, gpointer user_data);

static void
project_view_init(ProjectView *self)
{
  GtkCellRenderer *renderer;

  self->asdf = NULL;
  self->project = NULL;
  self->app = NULL;
  self->store = gtk_tree_store_new(PROJECT_VIEW_N_COLS,
      G_TYPE_STRING, G_TYPE_INT, G_TYPE_POINTER);
  gtk_tree_view_set_model(GTK_TREE_VIEW(self), GTK_TREE_MODEL(self->store));

  renderer = gtk_cell_renderer_text_new();
  gtk_tree_view_insert_column_with_attributes(GTK_TREE_VIEW(self), -1, NULL,
      renderer, "text", PROJECT_VIEW_COL_TEXT, NULL);
  gtk_tree_view_set_headers_visible(GTK_TREE_VIEW(self), FALSE);
  g_signal_connect(self, "button-press-event",
      G_CALLBACK(on_button_press), self);
}

static void
project_view_dispose(GObject *object)
{
  ProjectView *self = PROJECT_VIEW(object);
  g_clear_object(&self->asdf);
  if (self->project)
    project_set_package_added_cb(self->project, NULL, NULL);
  g_clear_pointer(&self->project, project_unref);
  g_clear_object(&self->app);
  g_clear_object(&self->store);
  G_OBJECT_CLASS(project_view_parent_class)->dispose(object);
}

static void
project_view_class_init(ProjectViewClass *klass)
{
  GObjectClass *object_class = G_OBJECT_CLASS(klass);
  object_class->dispose = project_view_dispose;
}

static void
project_view_populate_store(ProjectView *self)
{
  GtkTreeIter root;
  GtkTreeIter iter;
  GtkTreeIter child;
  const gchar *filename = asdf_get_filename(self->asdf);
  gchar *basename = filename ? g_path_get_basename(filename) : g_strdup("");
  gtk_tree_store_clear(self->store);

  gtk_tree_store_append(self->store, &root, NULL);
  gtk_tree_store_set(self->store, &root,
      PROJECT_VIEW_COL_TEXT, basename,
      PROJECT_VIEW_COL_KIND, PROJECT_VIEW_KIND_ROOT,
      PROJECT_VIEW_COL_OBJECT, self->project,
      -1);
  g_free(basename);

  gtk_tree_store_append(self->store, &iter, &root);
  gtk_tree_store_set(self->store, &iter,
      PROJECT_VIEW_COL_TEXT, "src",
      PROJECT_VIEW_COL_KIND, PROJECT_VIEW_KIND_SRC,
      PROJECT_VIEW_COL_OBJECT, self->project,
      -1);
  for (guint i = 0; i < asdf_get_component_count(self->asdf); i++) {
    const gchar *comp = asdf_get_component(self->asdf, i);
    ProjectFile *pf = NULL;
    if (self->project) {
      for (guint j = 0; j < project_get_file_count(self->project); j++) {
        ProjectFile *f = project_get_file(self->project, j);
        const gchar *rel = project_file_get_relative_path(f);
        if (filename_matches(comp, rel)) {
          pf = f;
          break;
        }
      }
    }
    gtk_tree_store_append(self->store, &child, &iter);
    gtk_tree_store_set(self->store, &child,
        PROJECT_VIEW_COL_TEXT, comp,
        PROJECT_VIEW_COL_KIND, PROJECT_VIEW_KIND_COMPONENT,
        PROJECT_VIEW_COL_OBJECT, pf,
        -1);
  }

  gtk_tree_store_append(self->store, &iter, &root);
  gtk_tree_store_set(self->store, &iter,
      PROJECT_VIEW_COL_TEXT, "packages",
      PROJECT_VIEW_COL_KIND, PROJECT_VIEW_KIND_PACKAGES,
      PROJECT_VIEW_COL_OBJECT, NULL,
      -1);
  if (self->project) {
    guint n = 0;
    gchar **names = project_get_package_names(self->project, &n);
    for (guint i = 0; i < n; i++) {
      const gchar *name = names[i];
      gtk_tree_store_append(self->store, &child, &iter);
      gtk_tree_store_set(self->store, &child,
          PROJECT_VIEW_COL_TEXT, name,
          PROJECT_VIEW_COL_KIND, PROJECT_VIEW_KIND_PACKAGE,
          PROJECT_VIEW_COL_OBJECT,
          project_get_package(self->project, name),
          -1);
    }
    g_free(names);
  }

  gtk_tree_view_expand_all(GTK_TREE_VIEW(self));
}

GtkWidget *
project_view_new(Asdf *asdf, App *app)
{
  g_return_val_if_fail(asdf != NULL, NULL);
  ProjectView *self = g_object_new(PROJECT_VIEW_TYPE, NULL);
  self->asdf = g_object_ref(asdf);
  self->app = app ? g_object_ref(app) : NULL;
  self->project = app ? project_ref(app_get_project(app)) : NULL;
  if (self->project)
    project_set_package_added_cb(self->project, on_package_added, self);
  project_view_populate_store(self);
  return GTK_WIDGET(self);
}

static gboolean
on_button_press(GtkWidget *widget, GdkEventButton *event, gpointer data)
{
  ProjectView *self = PROJECT_VIEW(data);
  if (event->type == GDK_BUTTON_PRESS && event->button == GDK_BUTTON_SECONDARY) {
    GtkTreePath *path = NULL;
    if (gtk_tree_view_get_path_at_pos(GTK_TREE_VIEW(widget), event->x, event->y,
        &path, NULL, NULL, NULL)) {
      GtkTreeSelection *sel = gtk_tree_view_get_selection(GTK_TREE_VIEW(widget));
      gtk_tree_selection_select_path(sel, path);
      GtkTreeIter iter;
      GtkTreeModel *model = gtk_tree_view_get_model(GTK_TREE_VIEW(widget));
      if (gtk_tree_model_get_iter(model, &iter, path)) {
        gint kind = 0;
        gtk_tree_model_get(model, &iter, PROJECT_VIEW_COL_KIND, &kind, -1);
        GtkWidget *menu = NULL;
        if (kind == PROJECT_VIEW_KIND_COMPONENT) {
          menu = gtk_menu_new();
          GtkWidget *rename = gtk_menu_item_new_with_label("Rename file");
          g_signal_connect(rename, "activate", G_CALLBACK(file_rename), self->app);
          gtk_menu_shell_append(GTK_MENU_SHELL(menu), rename);
          GtkWidget *del = gtk_menu_item_new_with_label("Delete file");
          g_signal_connect(del, "activate", G_CALLBACK(file_delete), self->app);
          gtk_menu_shell_append(GTK_MENU_SHELL(menu), del);
        } else if (kind == PROJECT_VIEW_KIND_ROOT || kind == PROJECT_VIEW_KIND_SRC) {
          menu = gtk_menu_new();
          GtkWidget *newf = gtk_menu_item_new_with_label("New file");
          g_signal_connect(newf, "activate", G_CALLBACK(file_new), self->app);
          gtk_menu_shell_append(GTK_MENU_SHELL(menu), newf);
          GtkWidget *add = gtk_menu_item_new_with_label("Add file");
          g_signal_connect(add, "activate", G_CALLBACK(file_add), self->app);
          gtk_menu_shell_append(GTK_MENU_SHELL(menu), add);
        }
        gtk_tree_path_free(path);
        if (menu) {
          gtk_widget_show_all(menu);
          gtk_menu_popup_at_pointer(GTK_MENU(menu), (GdkEvent *) event);
          return TRUE;
        }
        return FALSE;
      }
      gtk_tree_path_free(path);
    }
  }
  return GTK_WIDGET_CLASS(project_view_parent_class)->button_press_event(widget, event);
}

static gboolean
dispatch_package_added(gpointer data)
{
  g_assert(g_main_context_is_owner(g_main_context_default()));
  ProjectView *self = PROJECT_VIEW(data);
  project_view_populate_store(self);
  g_object_unref(self);
  return FALSE;
}

static void
on_package_added(Project * /*project*/, Package * /*package*/, gpointer user_data)
{
  ProjectView *self = PROJECT_VIEW(user_data);
  g_main_context_invoke(NULL, dispatch_package_added, g_object_ref(self));
}

static gboolean
filename_matches(const gchar *component, const gchar *file)
{
  if (!component || !file)
    return FALSE;
  if (g_strcmp0(component, file) == 0)
    return TRUE;
  gchar *base = g_path_get_basename(file);
  gchar *dot = g_strrstr(base, ".");
  if (dot)
    *dot = '\0';
  gboolean match = g_strcmp0(component, base) == 0;
  g_free(base);
  return match;
}

static gchar *
get_selected_component(ProjectView *self)
{
  GtkTreeSelection *selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(self));
  GtkTreeModel *model;
  GtkTreeIter iter;
  if (!gtk_tree_selection_get_selected(selection, &model, &iter))
    return NULL;
  gint kind = 0;
  gtk_tree_model_get(model, &iter, PROJECT_VIEW_COL_KIND, &kind, -1);
  if (kind != PROJECT_VIEW_KIND_COMPONENT)
    return NULL;
  gchar *comp = NULL;
  gtk_tree_model_get(model, &iter, PROJECT_VIEW_COL_TEXT, &comp, -1);
  return comp;
}

void
project_view_select_file(ProjectView *self, const gchar *file)
{
  g_return_if_fail(PROJECT_IS_VIEW(self));
  g_return_if_fail(file != NULL);
  gchar *current = get_selected_component(self);
  if (filename_matches(current, file)) {
    g_free(current);
    return;
  }
  g_free(current);
  GtkTreeModel *model = GTK_TREE_MODEL(self->store);
  GtkTreeIter root;
  GtkTreeIter iter;
  GtkTreeIter child;
  if (!gtk_tree_model_get_iter_first(model, &root))
    return;
  if (!gtk_tree_model_iter_children(model, &iter, &root))
    return;
  do {
    gint kind = 0;
    gtk_tree_model_get(model, &iter, PROJECT_VIEW_COL_KIND, &kind, -1);
    if (kind == PROJECT_VIEW_KIND_SRC) {
      if (gtk_tree_model_iter_children(model, &child, &iter)) {
        do {
          gchar *comp = NULL;
          gtk_tree_model_get(model, &child, PROJECT_VIEW_COL_TEXT, &comp, -1);
          if (filename_matches(comp, file)) {
            GtkTreeSelection *sel = gtk_tree_view_get_selection(GTK_TREE_VIEW(self));
            GtkTreePath *path = gtk_tree_model_get_path(model, &child);
            gtk_tree_selection_select_path(sel, path);
            gtk_tree_view_scroll_to_cell(GTK_TREE_VIEW(self), path, NULL, FALSE, 0, 0);
            gtk_tree_path_free(path);
            g_free(comp);
            return;
          }
          g_free(comp);
        } while (gtk_tree_model_iter_next(model, &child));
      }
      break;
    }
  } while (gtk_tree_model_iter_next(model, &iter));
}

