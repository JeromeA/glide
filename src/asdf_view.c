#include "asdf_view.h"
#include "project_file.h"
#include "app.h"
#include "file_new.h"
#include "file_add.h"
#include "file_delete.h"
#include "file_rename.h"

struct _AsdfView {
  GtkTreeView parent_instance;
  Asdf *asdf;
  Project *project;
  App *app;
  GtkTreeStore *store;
};

G_DEFINE_TYPE(AsdfView, asdf_view, GTK_TYPE_TREE_VIEW)

static void asdf_view_populate_store(AsdfView *self);
static gboolean filename_matches(const gchar *component, const gchar *file);
static gchar *get_selected_component(AsdfView *self);
static gboolean on_button_press(GtkWidget *widget, GdkEventButton *event, gpointer data);

static void
asdf_view_init(AsdfView *self)
{
  GtkCellRenderer *renderer;

  self->asdf = NULL;
  self->project = NULL;
  self->app = NULL;
  self->store = gtk_tree_store_new(ASDF_VIEW_N_COLS,
      G_TYPE_STRING, G_TYPE_INT, G_TYPE_POINTER);
  gtk_tree_view_set_model(GTK_TREE_VIEW(self), GTK_TREE_MODEL(self->store));

  renderer = gtk_cell_renderer_text_new();
  gtk_tree_view_insert_column_with_attributes(GTK_TREE_VIEW(self), -1, NULL,
      renderer, "text", ASDF_VIEW_COL_TEXT, NULL);
  gtk_tree_view_set_headers_visible(GTK_TREE_VIEW(self), FALSE);
  g_signal_connect(self, "button-press-event",
      G_CALLBACK(on_button_press), self);
}

static void
asdf_view_dispose(GObject *object)
{
  AsdfView *self = ASDF_VIEW(object);
  g_clear_object(&self->asdf);
  g_clear_pointer(&self->project, project_unref);
  g_clear_object(&self->app);
  g_clear_object(&self->store);
  G_OBJECT_CLASS(asdf_view_parent_class)->dispose(object);
}

static void
asdf_view_class_init(AsdfViewClass *klass)
{
  GObjectClass *object_class = G_OBJECT_CLASS(klass);
  object_class->dispose = asdf_view_dispose;
}

static void
asdf_view_populate_store(AsdfView *self)
{
  GtkTreeIter root;
  GtkTreeIter iter;
  GtkTreeIter child;
  const gchar *filename = asdf_get_filename(self->asdf);
  gchar *basename = filename ? g_path_get_basename(filename) : g_strdup("");
  gtk_tree_store_clear(self->store);

  gtk_tree_store_append(self->store, &root, NULL);
  gtk_tree_store_set(self->store, &root,
      ASDF_VIEW_COL_TEXT, basename,
      ASDF_VIEW_COL_KIND, ASDF_VIEW_KIND_ROOT,
      ASDF_VIEW_COL_OBJECT, self->project,
      -1);
  g_free(basename);

  gtk_tree_store_append(self->store, &iter, &root);
  gtk_tree_store_set(self->store, &iter,
      ASDF_VIEW_COL_TEXT, "src",
      ASDF_VIEW_COL_KIND, ASDF_VIEW_KIND_SRC,
      ASDF_VIEW_COL_OBJECT, self->project,
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
        ASDF_VIEW_COL_TEXT, comp,
        ASDF_VIEW_COL_KIND, ASDF_VIEW_KIND_COMPONENT,
        ASDF_VIEW_COL_OBJECT, pf,
        -1);
  }

  gtk_tree_store_append(self->store, &iter, &root);
  gtk_tree_store_set(self->store, &iter,
      ASDF_VIEW_COL_TEXT, "libraries",
      ASDF_VIEW_COL_KIND, ASDF_VIEW_KIND_LIBRARIES,
      ASDF_VIEW_COL_OBJECT, NULL,
      -1);
  gtk_tree_store_append(self->store, &child, &iter);
  gtk_tree_store_set(self->store, &child,
      ASDF_VIEW_COL_TEXT, "COMMON-LISP",
      ASDF_VIEW_COL_KIND, ASDF_VIEW_KIND_LIBRARY,
      ASDF_VIEW_COL_OBJECT, "COMMON-LISP",
      -1);
  for (guint i = 0; i < asdf_get_dependency_count(self->asdf); i++) {
    const gchar *dep = asdf_get_dependency(self->asdf, i);
    if (g_strcmp0(dep, "COMMON-LISP") != 0) {
      gtk_tree_store_append(self->store, &child, &iter);
      gtk_tree_store_set(self->store, &child,
          ASDF_VIEW_COL_TEXT, dep,
          ASDF_VIEW_COL_KIND, ASDF_VIEW_KIND_LIBRARY,
          ASDF_VIEW_COL_OBJECT, dep,
          -1);
    }
  }

  gtk_tree_view_expand_all(GTK_TREE_VIEW(self));
}

GtkWidget *
asdf_view_new(Asdf *asdf, App *app)
{
  g_return_val_if_fail(asdf != NULL, NULL);
  AsdfView *self = g_object_new(ASDF_VIEW_TYPE, NULL);
  self->asdf = g_object_ref(asdf);
  self->app = app ? g_object_ref(app) : NULL;
  self->project = app ? project_ref(app_get_project(app)) : NULL;
  asdf_view_populate_store(self);
  return GTK_WIDGET(self);
}

static gboolean
on_button_press(GtkWidget *widget, GdkEventButton *event, gpointer data)
{
  AsdfView *self = ASDF_VIEW(data);
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
        gtk_tree_model_get(model, &iter, ASDF_VIEW_COL_KIND, &kind, -1);
        GtkWidget *menu = NULL;
        if (kind == ASDF_VIEW_KIND_COMPONENT) {
          menu = gtk_menu_new();
          GtkWidget *rename = gtk_menu_item_new_with_label("Rename file");
          g_signal_connect(rename, "activate", G_CALLBACK(file_rename), self->app);
          gtk_menu_shell_append(GTK_MENU_SHELL(menu), rename);
          GtkWidget *del = gtk_menu_item_new_with_label("Delete file");
          g_signal_connect(del, "activate", G_CALLBACK(file_delete), self->app);
          gtk_menu_shell_append(GTK_MENU_SHELL(menu), del);
        } else if (kind == ASDF_VIEW_KIND_ROOT || kind == ASDF_VIEW_KIND_SRC) {
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
  return GTK_WIDGET_CLASS(asdf_view_parent_class)->button_press_event(widget, event);
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
get_selected_component(AsdfView *self)
{
  GtkTreeSelection *selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(self));
  GtkTreeModel *model;
  GtkTreeIter iter;
  if (!gtk_tree_selection_get_selected(selection, &model, &iter))
    return NULL;
  gint kind = 0;
  gtk_tree_model_get(model, &iter, ASDF_VIEW_COL_KIND, &kind, -1);
  if (kind != ASDF_VIEW_KIND_COMPONENT)
    return NULL;
  gchar *comp = NULL;
  gtk_tree_model_get(model, &iter, ASDF_VIEW_COL_TEXT, &comp, -1);
  return comp;
}

void
asdf_view_select_file(AsdfView *self, const gchar *file)
{
  g_return_if_fail(ASDF_IS_VIEW(self));
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
    gtk_tree_model_get(model, &iter, ASDF_VIEW_COL_KIND, &kind, -1);
    if (kind == ASDF_VIEW_KIND_SRC) {
      if (gtk_tree_model_iter_children(model, &child, &iter)) {
        do {
          gchar *comp = NULL;
          gtk_tree_model_get(model, &child, ASDF_VIEW_COL_TEXT, &comp, -1);
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

