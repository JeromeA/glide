#include "project.h"

struct _ProjectFile {
  ProjectFileState state;
  gchar *path;
  GtkTextBuffer *buffer; /* nullable */
  TextProvider *provider; /* owned */
  LispParser *parser; /* owned */
};

struct _Project {
  GObject parent_instance;
  GPtrArray *files; /* ProjectFile* */
};

static void project_finalize(GObject *obj);

G_DEFINE_TYPE(Project, project, G_TYPE_OBJECT)

static void project_class_init(ProjectClass *klass) {
  GObjectClass *obj = G_OBJECT_CLASS(klass);
  obj->finalize = project_finalize;
}

static void project_file_free(ProjectFile *file) {
  if (!file) return;
  if (file->parser) lisp_parser_free(file->parser);
  if (file->provider) g_object_unref(file->provider);
  if (file->buffer) g_object_unref(file->buffer);
  g_free(file->path);
  g_free(file);
}

static void project_init(Project *self) {
  self->files = g_ptr_array_new_with_free_func((GDestroyNotify)project_file_free);
}

static void project_finalize(GObject *obj) {
  Project *self = GLIDE_PROJECT(obj);
  if (self->files)
    g_ptr_array_free(self->files, TRUE);
  G_OBJECT_CLASS(project_parent_class)->finalize(obj);
}

Project *project_new(void) {
  return g_object_new(PROJECT_TYPE, NULL);
}

ProjectFile *project_add_file(Project *self, TextProvider *provider,
    GtkTextBuffer *buffer, const gchar *path, ProjectFileState state) {
  g_return_val_if_fail(GLIDE_IS_PROJECT(self), NULL);
  g_return_val_if_fail(GLIDE_IS_TEXT_PROVIDER(provider), NULL);

  ProjectFile *file = g_new0(ProjectFile, 1);
  file->state = state;
  file->provider = g_object_ref(provider);
  file->buffer = buffer ? g_object_ref(buffer) : NULL;
  file->parser = lisp_parser_new(file->provider);
  file->path = path ? g_strdup(path) : NULL;

  g_ptr_array_add(self->files, file);

  project_file_changed(self, file);

  return file;
}

void project_file_changed(Project *self /*unused*/, ProjectFile *file) {
  g_return_if_fail(file != NULL);
  if (file->parser)
    lisp_parser_parse(file->parser);
}

LispParser *project_file_get_parser(ProjectFile *file) {
  g_return_val_if_fail(file != NULL, NULL);
  return file->parser;
}

GtkTextBuffer *project_file_get_buffer(ProjectFile *file) {
  g_return_val_if_fail(file != NULL, NULL);
  return file->buffer;
}

