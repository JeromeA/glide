#include "project_view.h"
#include <gdk-pixbuf/gdk-pixbuf.h>
#include "document.h"
#include "project.h"
#include "app.h"
#include "file_new.h"
#include "file_add.h"
#include "file_delete.h"
#include "file_rename.h"
#include "util.h"

#define CSS_CLASS_PROJECT_VIEW "project-view"

struct _ProjectView {
  GtkTreeView parent_instance;
  Asdf *asdf;
  Project *project;
  App *app;
  GtkTreeStore *store;
  GdkPixbuf *icon_folder;
  GdkPixbuf *icon_function;
  GdkPixbuf *icon_package;
  GdkPixbuf *icon_variable;
  GdkPixbuf *icon_lisp;
  guint project_changed_source;
};

G_DEFINE_TYPE(ProjectView, project_view, GTK_TYPE_TREE_VIEW)

static void project_view_populate_store(ProjectView *self);
static void project_view_append_root(ProjectView *self, GtkTreeIter *root);
static void project_view_append_source_folder(ProjectView *self, GtkTreeIter *root);
static void project_view_append_component(ProjectView *self, GtkTreeIter *parent,
    const GString *component);
static Document *project_view_find_component_file(ProjectView *self,
    const gchar *component);
static void project_view_append_package_folder(ProjectView *self, GtkTreeIter *root);
static void project_view_append_package(ProjectView *self, GtkTreeIter *parent,
    const gchar *name);
static void project_view_append_functions(ProjectView *self, GtkTreeIter *parent,
    const gchar *package);
static void project_view_append_variables(ProjectView *self, GtkTreeIter *parent,
    const gchar *package);
static gboolean filename_matches(const gchar *component, const gchar *file);
static gchar *get_selected_component(ProjectView *self);
static gboolean on_button_press(GtkWidget *widget, GdkEventButton *event, gpointer data);
static gboolean dispatch_project_changed(gpointer data);
static gboolean schedule_project_changed(gpointer data);
static void on_project_changed(Project *project, gpointer user_data);
static gint compare_names(gconstpointer a, gconstpointer b, gpointer user_data);
static GdkPixbuf *load_icon(const gchar *filename);
static gboolean project_view_on_query_tooltip(GtkWidget *widget, gint x, gint y,
    gboolean keyboard_mode, GtkTooltip *tooltip, gpointer /*user_data*/);

static void
project_view_init(ProjectView *self)
{
  GtkCellRenderer *icon_renderer;
  GtkCellRenderer *text_renderer;
  GtkTreeViewColumn *column;

  self->asdf = NULL;
  self->project = NULL;
  self->app = NULL;
  self->store = gtk_tree_store_new(PROJECT_VIEW_N_COLS,
      GDK_TYPE_PIXBUF, G_TYPE_STRING, G_TYPE_INT, G_TYPE_POINTER);
  gtk_tree_view_set_model(GTK_TREE_VIEW(self), GTK_TREE_MODEL(self->store));
  gtk_widget_set_has_tooltip(GTK_WIDGET(self), TRUE);
  g_signal_connect(self, "query-tooltip", G_CALLBACK(project_view_on_query_tooltip), self);

  column = gtk_tree_view_column_new();
  /* Use a single column with icon and text renderers so the icon
     appears to the left of the label. */
  icon_renderer = gtk_cell_renderer_pixbuf_new();
  gtk_tree_view_column_pack_start(column, icon_renderer, FALSE);
  gtk_tree_view_column_add_attribute(column, icon_renderer, "pixbuf",
      PROJECT_VIEW_COL_ICON);

  text_renderer = gtk_cell_renderer_text_new();
  gtk_tree_view_column_pack_start(column, text_renderer, TRUE);
  gtk_tree_view_column_add_attribute(column, text_renderer, "text",
      PROJECT_VIEW_COL_TEXT);
  gtk_tree_view_append_column(GTK_TREE_VIEW(self), column);
  gtk_tree_view_set_headers_visible(GTK_TREE_VIEW(self), FALSE);
  g_signal_connect(self, "button-press-event",
      G_CALLBACK(on_button_press), self);

  self->icon_folder = load_icon("icon-folder.svg");
  self->icon_function = load_icon("icon-function.svg");
  self->icon_package = load_icon("icon-package.svg");
  self->icon_variable = load_icon("icon-variable.svg");
  self->icon_lisp = load_icon("icon-lisp.svg");
  self->project_changed_source = 0;

  gtk_style_context_add_class(
      gtk_widget_get_style_context(GTK_WIDGET(self)),
      CSS_CLASS_PROJECT_VIEW);

  GtkCssProvider *provider = gtk_css_provider_new();
  gtk_css_provider_load_from_data(provider,
      "." CSS_CLASS_PROJECT_VIEW ".view:selected { background-color: #27b; }"
      " ." CSS_CLASS_PROJECT_VIEW ".view:selected:focus { background-color: #27b; }",
      -1, NULL);
  gtk_style_context_add_provider_for_screen(
      gdk_screen_get_default(),
      GTK_STYLE_PROVIDER(provider),
      GTK_STYLE_PROVIDER_PRIORITY_APPLICATION);
  g_object_unref(provider);
}

static void
project_view_dispose(GObject *object)
{
  ProjectView *self = PROJECT_VIEW(object);
  g_clear_object(&self->asdf);
  if (self->project)
    project_set_changed_cb(self->project, NULL, NULL);
  g_clear_pointer(&self->project, project_unref);
  g_clear_object(&self->app);
  g_clear_object(&self->store);
  g_clear_object(&self->icon_folder);
  g_clear_object(&self->icon_function);
  g_clear_object(&self->icon_package);
  g_clear_object(&self->icon_variable);
  g_clear_object(&self->icon_lisp);
  if (self->project_changed_source) {
    g_source_remove(self->project_changed_source);
    self->project_changed_source = 0;
  }
  G_OBJECT_CLASS(project_view_parent_class)->dispose(object);
}

static void
project_view_class_init(ProjectViewClass *klass)
{
  GObjectClass *object_class = G_OBJECT_CLASS(klass);
  object_class->dispose = project_view_dispose;
}

static gint
compare_names(gconstpointer a, gconstpointer b, gpointer /*user_data*/)
{
  const gchar *sa = *(const gchar *const *) a;
  const gchar *sb = *(const gchar *const *) b;
  return g_strcmp0(sa, sb);
}

static GdkPixbuf *
load_icon(const gchar *filename)
{
  g_return_val_if_fail(filename != NULL, NULL);
  GError *error = NULL;
  GdkPixbuf *pixbuf = gdk_pixbuf_new_from_file_at_scale(filename, 16, 16,
      TRUE, &error);
  if (pixbuf) {
    LOG(1, "Loaded icon %s", filename);
    return pixbuf;
  }
  g_clear_error(&error);
  gchar *path = g_build_filename("src", filename, NULL);
  pixbuf = gdk_pixbuf_new_from_file_at_scale(path, 16, 16, TRUE, &error);
  if (pixbuf)
    LOG(1, "Loaded icon %s", path);
  else
    g_warning("Failed to load %s: %s", path, error->message);
  g_clear_error(&error);
  g_free(path);
  return pixbuf;
}

static void
project_view_append_root(ProjectView *self, GtkTreeIter *root)
{
  const GString *filename = asdf_get_filename(self->asdf);
  gchar *basename = filename ? g_path_get_basename(filename->str) : g_strdup("");
  gtk_tree_store_append(self->store, root, NULL);
  gtk_tree_store_set(self->store, root,
      PROJECT_VIEW_COL_ICON, self->icon_folder,
      PROJECT_VIEW_COL_TEXT, basename,
      PROJECT_VIEW_COL_KIND, PROJECT_VIEW_KIND_ROOT,
      PROJECT_VIEW_COL_OBJECT, self->project,
      -1);
  g_free(basename);
}

static Document *
project_view_find_component_file(ProjectView *self, const gchar *component)
{
  if (!self->project)
    return NULL;
  for (guint j = 0; j < project_get_document_count(self->project); j++) {
    Document *document = project_get_document(self->project, j);
    const gchar *rel = document_get_relative_path(document);
    if (filename_matches(component, rel))
      return document;
  }
  return NULL;
}

static void
project_view_append_component(ProjectView *self, GtkTreeIter *parent,
    const GString *component)
{
  Document *document = project_view_find_component_file(self, component->str);
  GtkTreeIter child;
  gtk_tree_store_append(self->store, &child, parent);
  gtk_tree_store_set(self->store, &child,
      PROJECT_VIEW_COL_ICON, self->icon_lisp,
      PROJECT_VIEW_COL_TEXT, component->str,
      PROJECT_VIEW_COL_KIND, PROJECT_VIEW_KIND_SRC,
      PROJECT_VIEW_COL_OBJECT, document,
      -1);
}

static void
project_view_append_source_folder(ProjectView *self, GtkTreeIter *root)
{
  GtkTreeIter iter;
  gtk_tree_store_append(self->store, &iter, root);
  gtk_tree_store_set(self->store, &iter,
      PROJECT_VIEW_COL_ICON, self->icon_folder,
      PROJECT_VIEW_COL_TEXT, "src",
      PROJECT_VIEW_COL_KIND, PROJECT_VIEW_KIND_SRC_FOLDER,
      PROJECT_VIEW_COL_OBJECT, self->project,
      -1);
  for (guint i = 0; i < asdf_get_component_count(self->asdf); i++) {
    const GString *comp = asdf_get_component(self->asdf, i);
    project_view_append_component(self, &iter, comp);
  }
}

static void
project_view_append_functions(ProjectView *self, GtkTreeIter *parent,
    const gchar *package)
{
  guint fn = 0;
  gchar **fnames = project_get_function_names(self->project, package, &fn);
  if (!fnames)
    return;
  g_qsort_with_data(fnames, fn, sizeof(gchar *), compare_names, NULL);
  GtkTreeIter grandchild;
  for (guint j = 0; j < fn; j++) {
    const gchar *fname = fnames[j];
    Function *func = project_get_function(self->project, fname);
    gtk_tree_store_append(self->store, &grandchild, parent);
    gtk_tree_store_set(self->store, &grandchild,
        PROJECT_VIEW_COL_ICON, self->icon_function,
        PROJECT_VIEW_COL_TEXT, fname,
        PROJECT_VIEW_COL_KIND, PROJECT_VIEW_KIND_FUNCTION,
        PROJECT_VIEW_COL_OBJECT, func,
        -1);
  }
  g_free(fnames);
}

static void
project_view_append_variables(ProjectView *self, GtkTreeIter *parent,
    const gchar *package)
{
  guint vn = 0;
  gchar **vnames = project_get_variable_names(self->project, package, &vn);
  if (!vnames)
    return;
  g_qsort_with_data(vnames, vn, sizeof(gchar *), compare_names, NULL);
  GtkTreeIter grandchild;
  for (guint j = 0; j < vn; j++) {
    const gchar *vname = vnames[j];
    gtk_tree_store_append(self->store, &grandchild, parent);
    gtk_tree_store_set(self->store, &grandchild,
        PROJECT_VIEW_COL_ICON, self->icon_variable,
        PROJECT_VIEW_COL_TEXT, vname,
        PROJECT_VIEW_COL_KIND, PROJECT_VIEW_KIND_VARIABLE,
        PROJECT_VIEW_COL_OBJECT, NULL,
        -1);
  }
  g_free(vnames);
}

static void
project_view_append_package(ProjectView *self, GtkTreeIter *parent,
    const gchar *name)
{
  if (!self->project)
    return;
  GtkTreeIter child;
  gtk_tree_store_append(self->store, &child, parent);
  gtk_tree_store_set(self->store, &child,
      PROJECT_VIEW_COL_ICON, self->icon_package,
      PROJECT_VIEW_COL_TEXT, name,
      PROJECT_VIEW_COL_KIND, PROJECT_VIEW_KIND_PACKAGE,
      PROJECT_VIEW_COL_OBJECT, project_get_package(self->project, name),
      -1);
  project_view_append_functions(self, &child, name);
  project_view_append_variables(self, &child, name);
}

static void
project_view_append_package_folder(ProjectView *self, GtkTreeIter *root)
{
  GtkTreeIter iter;
  gtk_tree_store_append(self->store, &iter, root);
  gtk_tree_store_set(self->store, &iter,
      PROJECT_VIEW_COL_ICON, self->icon_folder,
      PROJECT_VIEW_COL_TEXT, "packages",
      PROJECT_VIEW_COL_KIND, PROJECT_VIEW_KIND_PACKAGES,
      PROJECT_VIEW_COL_OBJECT, NULL,
      -1);
  if (!self->project)
    return;
  guint n = 0;
  gchar **names = project_get_package_names(self->project, &n);
  if (!names)
    return;
  g_qsort_with_data(names, n, sizeof(gchar *), compare_names, NULL);
  for (guint i = 0; i < n; i++)
    project_view_append_package(self, &iter, names[i]);
  g_free(names);
}

static void
project_view_populate_store(ProjectView *self)
{
  gtk_tree_store_clear(self->store);
  GtkTreeIter root;
  project_view_append_root(self, &root);
  project_view_append_source_folder(self, &root);
  project_view_append_package_folder(self, &root);
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
    project_set_changed_cb(self->project, on_project_changed, self);
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
        if (kind == PROJECT_VIEW_KIND_SRC) {
          menu = gtk_menu_new();
          GtkWidget *rename = gtk_menu_item_new_with_label("Rename file");
          g_signal_connect(rename, "activate", G_CALLBACK(file_rename), self->app);
          gtk_menu_shell_append(GTK_MENU_SHELL(menu), rename);
          GtkWidget *del = gtk_menu_item_new_with_label("Delete file");
          g_signal_connect(del, "activate", G_CALLBACK(file_delete), self->app);
          gtk_menu_shell_append(GTK_MENU_SHELL(menu), del);
        } else if (kind == PROJECT_VIEW_KIND_ROOT || kind == PROJECT_VIEW_KIND_SRC_FOLDER) {
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
project_view_on_query_tooltip(GtkWidget *widget, gint x, gint y, gboolean keyboard_mode,
    GtkTooltip *tooltip, gpointer /*user_data*/)
{
  g_assert(glide_is_ui_thread());
  LOG(1, "ProjectView.on_query_tooltip at %d,%d", x, y);
  GtkTreeModel *model;
  GtkTreePath *path;
  GtkTreeIter iter;
  if (!gtk_tree_view_get_tooltip_context(GTK_TREE_VIEW(widget), &x, &y,
      keyboard_mode, &model, &path, &iter)) {
    LOG(1, "ProjectView.on_query_tooltip: no context");
    return FALSE;
  }
  gint kind = 0;
  gpointer obj = NULL;
  gtk_tree_model_get(model, &iter,
      PROJECT_VIEW_COL_KIND, &kind,
      PROJECT_VIEW_COL_OBJECT, &obj,
      -1);
  LOG(1, "ProjectView.on_query_tooltip: kind=%d obj=%p", kind, obj);
  gboolean rv = FALSE;
  if (kind == PROJECT_VIEW_KIND_FUNCTION && obj) {
    gchar *tt = function_tooltip((Function *) obj);
    if (tt) {
      gtk_tree_view_set_tooltip_row(GTK_TREE_VIEW(widget), tooltip, path);
      gtk_tooltip_set_markup(tooltip, tt);
      g_free(tt);
      rv = TRUE;
    } else {
      LOG(1, "ProjectView.on_query_tooltip: empty tooltip");
    }
  } else {
    LOG(1, "ProjectView.on_query_tooltip: no tooltip for kind=%d", kind);
  }
  gtk_tree_path_free(path);
  LOG(1, "ProjectView.on_query_tooltip: rv=%d", rv);
  return rv;
}

static gboolean
dispatch_project_changed(gpointer data)
{
  g_assert(glide_is_ui_thread());
  ProjectView *self = PROJECT_VIEW(data);
  self->project_changed_source = 0;
  LOG(1, "ProjectView.dispatch_project_changed update started");
  project_view_populate_store(self);
  g_object_unref(self);
  return FALSE;
}

static gboolean
schedule_project_changed(gpointer data)
{
  g_assert(glide_is_ui_thread());
  ProjectView *self = PROJECT_VIEW(data);
  if (!self->project_changed_source) {
    LOG(1, "ProjectView.schedule_project_changed armed");
    self->project_changed_source = g_timeout_add(10, dispatch_project_changed, data);
  } else {
    LOG(1, "ProjectView.schedule_project_changed ignored");
    g_object_unref(self);
  }
  return FALSE;
}

static void
on_project_changed(Project * /*project*/, gpointer user_data)
{
  ProjectView *self = PROJECT_VIEW(user_data);
  g_main_context_invoke(NULL, schedule_project_changed, g_object_ref(self));
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
  if (kind != PROJECT_VIEW_KIND_SRC)
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
    if (kind == PROJECT_VIEW_KIND_SRC_FOLDER) {
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

