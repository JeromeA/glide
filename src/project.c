#include "project.h"
#include "string_text_provider.h"

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
  ProjectFile *current_file;
  guint next_scratch_id;
};

static void project_finalize(GObject *obj);
ProjectFile *project_create_scratch(Project *self);

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
  self->next_scratch_id = 0;
  self->current_file = NULL;
}

static void project_finalize(GObject *obj) {
  Project *self = GLIDE_PROJECT(obj);
  if (self->files)
    g_ptr_array_free(self->files, TRUE);
  self->current_file = NULL;
  G_OBJECT_CLASS(project_parent_class)->finalize(obj);
}

Project *project_new(void) {
  Project *self = g_object_new(PROJECT_TYPE, NULL);
  project_create_scratch(self);
  return self;
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

ProjectFile *project_create_scratch(Project *self) {
  g_return_val_if_fail(GLIDE_IS_PROJECT(self), NULL);
  gchar name[12];
  g_snprintf(name, sizeof(name), "scratch%02u", self->next_scratch_id++);
  TextProvider *provider = string_text_provider_new("");
  ProjectFile *file = project_add_file(self, provider, NULL, name,
      PROJECT_FILE_SCRATCH);
  g_object_unref(provider);
  project_set_current_file(self, file);
  return file;
}

guint project_get_file_count(Project *self) {
  g_return_val_if_fail(GLIDE_IS_PROJECT(self), 0);
  return self->files->len;
}

ProjectFile *project_get_file(Project *self, guint index) {
  g_return_val_if_fail(GLIDE_IS_PROJECT(self), NULL);
  if (index >= self->files->len)
    return NULL;
  return g_ptr_array_index(self->files, index);
}

ProjectFile *project_get_current_file(Project *self) {
  g_return_val_if_fail(GLIDE_IS_PROJECT(self), NULL);
  return self->current_file ? self->current_file : project_get_file(self, 0);
}

void project_set_current_file(Project *self, ProjectFile *file) {
  g_return_if_fail(GLIDE_IS_PROJECT(self));
  g_return_if_fail(file != NULL);
  self->current_file = file;
}

ProjectFileState project_file_get_state(ProjectFile *file) {
  g_return_val_if_fail(file != NULL, PROJECT_FILE_DORMANT);
  return file->state;
}

void project_file_set_state(ProjectFile *file, ProjectFileState state) {
  g_return_if_fail(file != NULL);
  file->state = state;
}

void project_file_set_provider(ProjectFile *file, TextProvider *provider,
    GtkTextBuffer *buffer) {
  g_return_if_fail(file != NULL);
  g_return_if_fail(GLIDE_IS_TEXT_PROVIDER(provider));
  if (file->parser)
    lisp_parser_free(file->parser);
  if (file->provider)
    g_object_unref(file->provider);
  if (file->buffer)
    g_object_unref(file->buffer);
  file->provider = g_object_ref(provider);
  file->buffer = buffer ? g_object_ref(buffer) : NULL;
  file->parser = lisp_parser_new(file->provider);
  project_file_changed(NULL, file);
}

TextProvider *project_file_get_provider(ProjectFile *file) {
  g_return_val_if_fail(file != NULL, NULL);
  return file->provider;
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

const gchar *project_file_get_path(ProjectFile *file) {
  g_return_val_if_fail(file != NULL, NULL);
  return file->path;
}

void project_file_set_path(ProjectFile *file, const gchar *path) {
  g_return_if_fail(file != NULL);
  g_free(file->path);
  file->path = path ? g_strdup(path) : NULL;
}

