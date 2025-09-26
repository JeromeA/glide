#include "editor_manager.h"
#include "document.h"
#include "util.h"

struct _EditorManager {
  GObject parent_instance;

  Project *project;
  EditorContainer *container;
  GHashTable *editors; /* Document* -> Editor* */
};

G_DEFINE_TYPE(EditorManager, editor_manager, G_TYPE_OBJECT)

static void editor_manager_add_document(EditorManager *self, Document *document, gboolean focus);
static void editor_manager_remove_document(EditorManager *self, Document *document);
static void on_document_loaded(Project *project, Document *document, gpointer user_data);
static void on_document_removed(Project *project, Document *document, gpointer user_data);
static void on_document_changed(Project *project, Document *document, gpointer user_data);

static void
editor_manager_init(EditorManager *self)
{
  self->project = NULL;
  self->container = NULL;
  self->editors = NULL;
}

static void
editor_manager_dispose(GObject *object)
{
  EditorManager *self = EDITOR_MANAGER(object);
  if (self->project) {
    project_set_document_loaded_cb(self->project, NULL, NULL);
    project_set_document_removed_cb(self->project, NULL, NULL);
    project_set_document_changed_cb(self->project, NULL, NULL);
    project_unref(self->project);
    self->project = NULL;
  }
  g_clear_object(&self->container);
  if (self->editors) {
    g_hash_table_destroy(self->editors);
    self->editors = NULL;
  }
  G_OBJECT_CLASS(editor_manager_parent_class)->dispose(object);
}

static void
editor_manager_class_init(EditorManagerClass *klass)
{
  GObjectClass *object_class = G_OBJECT_CLASS(klass);
  object_class->dispose = editor_manager_dispose;
}

EditorManager *
editor_manager_new(Project *project, EditorContainer *container)
{
  g_return_val_if_fail(project != NULL, NULL);
  g_return_val_if_fail(EDITOR_IS_CONTAINER(container), NULL);

  EditorManager *self = g_object_new(EDITOR_TYPE_MANAGER, NULL);
  self->project = project_ref(project);
  self->container = g_object_ref(container);
  self->editors = g_hash_table_new_full(g_direct_hash, g_direct_equal, NULL,
      (GDestroyNotify)g_object_unref);

  project_set_document_loaded_cb(project, on_document_loaded, self);
  project_set_document_removed_cb(project, on_document_removed, self);
  project_set_document_changed_cb(project, on_document_changed, self);

  guint count = project_get_document_count(project);
  for (guint i = 0; i < count; i++) {
    Document *document = project_get_document(project, i);
    editor_manager_add_document(self, document, FALSE);
  }

  return self;
}

static void
editor_manager_add_document(EditorManager *self, Document *document, gboolean focus)
{
  g_return_if_fail(EDITOR_IS_MANAGER(self));
  g_return_if_fail(document != NULL);
  if (g_hash_table_contains(self->editors, document))
    return;

  const gchar *path = document_get_path(document);
  LOG(1, "editor_manager_add_document path=%s focus=%d", path ? path : "(null)", focus);
  GtkWidget *widget = editor_new_for_document(self->project, document);
  Editor *editor = GLIDE_EDITOR(widget);
  g_hash_table_insert(self->editors, document, g_object_ref(editor));
  gint page = editor_container_add_editor(self->container, document, editor);
  gtk_widget_show_all(GTK_WIDGET(editor));
  if (focus)
    gtk_notebook_set_current_page(GTK_NOTEBOOK(self->container), page);
  editor_set_errors(editor, document_get_errors(document));
}

static void
editor_manager_remove_document(EditorManager *self, Document *document)
{
  g_return_if_fail(EDITOR_IS_MANAGER(self));
  g_return_if_fail(document != NULL);
  const gchar *path = document_get_path(document);
  LOG(1, "editor_manager_remove_document path=%s", path ? path : "(null)");
  editor_container_remove_document(self->container, document);
  g_hash_table_remove(self->editors, document);
}

static void
on_document_loaded(Project * /*project*/, Document *document, gpointer user_data)
{
  EditorManager *self = EDITOR_MANAGER(user_data);
  editor_manager_add_document(self, document, TRUE);
}

static void
on_document_removed(Project * /*project*/, Document *document, gpointer user_data)
{
  EditorManager *self = EDITOR_MANAGER(user_data);
  editor_manager_remove_document(self, document);
}

static void
on_document_changed(Project * /*project*/, Document *document, gpointer user_data)
{
  EditorManager *self = EDITOR_MANAGER(user_data);
  const gchar *path = document_get_path(document);
  LOG(1, "editor_manager_file_changed path=%s", path ? path : "(null)");
  Editor *editor = editor_manager_get_editor(self, document);
  if (!editor)
    return;
  editor_set_errors(editor, document_get_errors(document));
}

Editor *
editor_manager_get_editor(EditorManager *self, Document *document)
{
  g_return_val_if_fail(EDITOR_IS_MANAGER(self), NULL);
  if (!document || !self->editors)
    return NULL;
  return g_hash_table_lookup(self->editors, document);
}

GtkTextBuffer *
editor_manager_get_buffer(EditorManager *self, Document *document)
{
  Editor *editor = editor_manager_get_editor(self, document);
  if (!editor)
    return NULL;
  return GTK_TEXT_BUFFER(editor_get_buffer(editor));
}
