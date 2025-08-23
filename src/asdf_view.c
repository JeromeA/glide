#include "asdf_view.h"

struct _AsdfView {
  GtkTreeView parent_instance;
  Asdf *asdf;
  GtkTreeStore *store;
};

G_DEFINE_TYPE(AsdfView, asdf_view, GTK_TYPE_TREE_VIEW)

enum { COL_TEXT, N_COLS };

static void populate_store(AsdfView *self);

static void
asdf_view_init(AsdfView *self)
{
  GtkCellRenderer *renderer;

  self->asdf = NULL;
  self->store = gtk_tree_store_new(N_COLS, G_TYPE_STRING);
  gtk_tree_view_set_model(GTK_TREE_VIEW(self), GTK_TREE_MODEL(self->store));

  renderer = gtk_cell_renderer_text_new();
  gtk_tree_view_insert_column_with_attributes(GTK_TREE_VIEW(self), -1, NULL,
      renderer, "text", COL_TEXT, NULL);
  gtk_tree_view_set_headers_visible(GTK_TREE_VIEW(self), FALSE);
}

static void
asdf_view_dispose(GObject *object)
{
  AsdfView *self = ASDF_VIEW(object);
  g_clear_object(&self->asdf);
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
populate_store(AsdfView *self)
{
  GtkTreeIter root;
  GtkTreeIter iter;
  GtkTreeIter child;
  const gchar *pathname = asdf_get_pathname(self->asdf);
  const gchar *filename = asdf_get_filename(self->asdf);
  gchar *basename = filename ? g_path_get_basename(filename) : g_strdup("");
  gtk_tree_store_clear(self->store);

  gtk_tree_store_append(self->store, &root, NULL);
  gtk_tree_store_set(self->store, &root, COL_TEXT, basename, -1);
  g_free(basename);

  gtk_tree_store_append(self->store, &iter, &root);
  gchar *text = g_strdup_printf("pathname %s", pathname ? pathname : "");
  gtk_tree_store_set(self->store, &iter, COL_TEXT, text, -1);
  g_free(text);

  gtk_tree_store_append(self->store, &iter, &root);
  text = g_strdup_printf("serial %s", asdf_get_serial(self->asdf) ? "t" : "nil");
  gtk_tree_store_set(self->store, &iter, COL_TEXT, text, -1);
  g_free(text);

  gtk_tree_store_append(self->store, &iter, &root);
  gtk_tree_store_set(self->store, &iter, COL_TEXT, "depends-on", -1);
  for (guint i = 0; i < asdf_get_dependency_count(self->asdf); i++) {
    const gchar *dep = asdf_get_dependency(self->asdf, i);
    gtk_tree_store_append(self->store, &child, &iter);
    gtk_tree_store_set(self->store, &child, COL_TEXT, dep, -1);
  }

  gtk_tree_store_append(self->store, &iter, &root);
  gtk_tree_store_set(self->store, &iter, COL_TEXT, "components", -1);
  for (guint i = 0; i < asdf_get_component_count(self->asdf); i++) {
    const gchar *comp = asdf_get_component(self->asdf, i);
    gtk_tree_store_append(self->store, &child, &iter);
    gtk_tree_store_set(self->store, &child, COL_TEXT, comp, -1);
  }

  gtk_tree_view_expand_all(GTK_TREE_VIEW(self));
}

GtkWidget *
asdf_view_new(Asdf *asdf)
{
  g_return_val_if_fail(asdf != NULL, NULL);
  AsdfView *self = g_object_new(ASDF_VIEW_TYPE, NULL);
  self->asdf = g_object_ref(asdf);
  populate_store(self);
  return GTK_WIDGET(self);
}

