#include "editor_manager.h"
#include "project_file.h"
#include "util.h"

struct _EditorManager {
  GObject parent_instance;

  Project *project;
  EditorContainer *container;
  GHashTable *editors; /* ProjectFile* -> Editor* */
};

G_DEFINE_TYPE(EditorManager, editor_manager, G_TYPE_OBJECT)

static void editor_manager_add_file(EditorManager *self, ProjectFile *file, gboolean focus);
static void editor_manager_remove_file(EditorManager *self, ProjectFile *file);
static void on_project_file_loaded(Project *project, ProjectFile *file, gpointer user_data);
static void on_project_file_removed(Project *project, ProjectFile *file, gpointer user_data);
static void on_project_file_changed(Project *project, ProjectFile *file, gpointer user_data);

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
    project_set_file_loaded_cb(self->project, NULL, NULL);
    project_set_file_removed_cb(self->project, NULL, NULL);
    project_set_file_changed_cb(self->project, NULL, NULL);
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

  project_set_file_loaded_cb(project, on_project_file_loaded, self);
  project_set_file_removed_cb(project, on_project_file_removed, self);
  project_set_file_changed_cb(project, on_project_file_changed, self);

  guint count = project_get_file_count(project);
  for (guint i = 0; i < count; i++) {
    ProjectFile *file = project_get_file(project, i);
    editor_manager_add_file(self, file, FALSE);
  }

  return self;
}

static void
editor_manager_add_file(EditorManager *self, ProjectFile *file, gboolean focus)
{
  g_return_if_fail(EDITOR_IS_MANAGER(self));
  g_return_if_fail(file != NULL);
  if (g_hash_table_contains(self->editors, file))
    return;

  const gchar *path = project_file_get_path(file);
  LOG(1, "editor_manager_add_file path=%s focus=%d", path ? path : "(null)", focus);
  GtkWidget *widget = editor_new_for_file(self->project, file);
  Editor *editor = GLIDE_EDITOR(widget);
  g_hash_table_insert(self->editors, file, g_object_ref(editor));
  gint page = editor_container_add_editor(self->container, file, editor);
  gtk_widget_show_all(GTK_WIDGET(editor));
  if (focus)
    gtk_notebook_set_current_page(GTK_NOTEBOOK(self->container), page);
  editor_set_errors(editor, project_file_get_errors(file));
}

static void
editor_manager_remove_file(EditorManager *self, ProjectFile *file)
{
  g_return_if_fail(EDITOR_IS_MANAGER(self));
  g_return_if_fail(file != NULL);
  const gchar *path = project_file_get_path(file);
  LOG(1, "editor_manager_remove_file path=%s", path ? path : "(null)");
  editor_container_remove_file(self->container, file);
  g_hash_table_remove(self->editors, file);
}

static void
on_project_file_loaded(Project * /*project*/, ProjectFile *file, gpointer user_data)
{
  EditorManager *self = EDITOR_MANAGER(user_data);
  editor_manager_add_file(self, file, TRUE);
}

static void
on_project_file_removed(Project * /*project*/, ProjectFile *file, gpointer user_data)
{
  EditorManager *self = EDITOR_MANAGER(user_data);
  editor_manager_remove_file(self, file);
}

static void
on_project_file_changed(Project * /*project*/, ProjectFile *file, gpointer user_data)
{
  EditorManager *self = EDITOR_MANAGER(user_data);
  const gchar *path = project_file_get_path(file);
  LOG(1, "editor_manager_file_changed path=%s", path ? path : "(null)");
  Editor *editor = editor_manager_get_editor(self, file);
  if (!editor)
    return;
  editor_set_errors(editor, project_file_get_errors(file));
}

Editor *
editor_manager_get_editor(EditorManager *self, ProjectFile *file)
{
  g_return_val_if_fail(EDITOR_IS_MANAGER(self), NULL);
  if (!file || !self->editors)
    return NULL;
  return g_hash_table_lookup(self->editors, file);
}

GtkTextBuffer *
editor_manager_get_buffer(EditorManager *self, ProjectFile *file)
{
  Editor *editor = editor_manager_get_editor(self, file);
  if (!editor)
    return NULL;
  return GTK_TEXT_BUFFER(editor_get_buffer(editor));
}
