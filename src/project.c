#include "project.h"
#include "analyse.h"
#include "util.h"
#include "project_repl.h"
#include "project_index.h"
#include <glib-object.h>

struct _Project {
  GPtrArray *documents; /* Document* */
  ProjectIndex *index;
  GPtrArray *event_handlers; /* ProjectEventHandler* */
  guint next_event_handler_id;
  Asdf *asdf; /* owned, nullable */
  ProjectRepl *repl;
  gchar *path;
  gint refcnt;
};

typedef struct {
  ProjectEventCb callback;
  gpointer user_data;
  guint id;
} ProjectEventHandler;

static void project_emit_event(Project *self, ProjectChangeEventType type, Document *document);

static Project *project_init(void) {
  Project *self = g_new0(Project, 1);
  self->refcnt = 1;
  self->documents = g_ptr_array_new_with_free_func((GDestroyNotify)document_free);
  self->index = project_index_new();
  self->asdf = asdf_new();
  self->repl = NULL;
  self->path = NULL;
  self->event_handlers = g_ptr_array_new_with_free_func(g_free);
  self->next_event_handler_id = 1;
  return self;
}

static void project_free(Project *self) {
  g_clear_pointer(&self->index, project_index_free);
  if (self->documents)
    g_ptr_array_free(self->documents, TRUE);
  g_clear_object(&self->asdf);
  project_repl_free(self->repl);
  g_free(self->path);
  if (self->event_handlers)
    g_ptr_array_unref(self->event_handlers);
  g_free(self);
}

Project *project_new(ReplSession *repl) {
  g_return_val_if_fail(glide_is_ui_thread(), NULL);
  LOG(1, "project_new");
  Project *self = project_init();
  self->repl = repl ? project_repl_new(self, repl) : NULL;
  project_clear(self);
  GString *content = g_string_new("");
  project_add_document(self, content, "unnamed.lisp", DOCUMENT_LIVE);
  return self;
}

Document *project_add_document(Project *self, GString *content,
    const gchar *path, DocumentState state) {
  g_return_val_if_fail(self != NULL, NULL);
  g_return_val_if_fail(glide_is_ui_thread(), NULL);

  LOG(1, "project_add_document path=%s state=%d", path ? path : "(null)", state);

  if (!content)
    content = g_string_new("");
  Document *document = document_new(self, path, state);
  g_ptr_array_add(self->documents, document);
  document_set_content(document, content);

  project_document_loaded(self, document);
  return document;
}

Document *project_add_loaded_document(Project *self, const gchar *path) {
  g_return_val_if_fail(self != NULL, NULL);
  g_return_val_if_fail(path != NULL, NULL);
  g_return_val_if_fail(glide_is_ui_thread(), NULL);

  LOG(1, "project_add_loaded_document path=%s", path);

  GString *content = document_load_buffer(path);
  if (!content)
    return NULL;

  Document *document = document_new(self, path, DOCUMENT_LIVE);
  g_ptr_array_add(self->documents, document);
  document_set_content(document, content);
  project_document_loaded(self, document);
  return document;
}

void project_remove_document(Project *self, Document *document) {
  g_return_if_fail(self != NULL);
  g_return_if_fail(document != NULL);
  g_return_if_fail(glide_is_ui_thread());
  for (guint i = 0; i < self->documents->len; i++) {
    if (g_ptr_array_index(self->documents, i) == document) {
      project_document_removed(self, document);
      g_ptr_array_remove_index(self->documents, i);
      break;
    }
  }
  project_index_clear(self->index);
  for (guint i = 0; i < self->documents->len; i++) {
    Document *f = g_ptr_array_index(self->documents, i);
    const Node *a = document_get_ast(f);
    if (a)
      project_index_walk(self->index, a);
  }

  project_changed(self);
}

guint project_get_document_count(Project *self) {
  g_return_val_if_fail(self != NULL, 0);
  return self->documents->len;
}

Document *project_get_document(Project *self, guint index) {
  g_return_val_if_fail(self != NULL, NULL);
  if (index >= self->documents->len)
    return NULL;
  return g_ptr_array_index(self->documents, index);
}

GHashTable *project_get_index(Project *self, StringDesignatorType sd_type) {
  g_return_val_if_fail(self != NULL, NULL);
  return project_index_get(self->index, sd_type);
}

void project_add_package(Project *self, Package *package) {
  g_return_if_fail(self != NULL);
  g_return_if_fail(package != NULL);
  g_return_if_fail(glide_is_ui_thread());
  project_index_add_package(self->index, package);
  project_changed(self);
}

void project_add_function(Project *self, Function *function) {
  g_return_if_fail(self != NULL);
  g_return_if_fail(function != NULL);
  g_return_if_fail(glide_is_ui_thread());
  project_index_add_function(self->index, function);
  project_changed(self);
}

Function *project_get_function(Project *self, const gchar *name) {
  g_return_val_if_fail(self != NULL, NULL);
  g_return_val_if_fail(name != NULL, NULL);
  return project_index_get_function(self->index, name);
}

gchar **project_get_function_names(Project *self, const gchar *package,
    guint *length) {
  g_return_val_if_fail(self != NULL, NULL);
  return project_index_get_function_names(self->index, package, length);
}

void project_add_variable(Project *self, const gchar *package,
    const gchar *name, const gchar *doc) {
  g_return_if_fail(self != NULL);
  g_return_if_fail(package != NULL);
  g_return_if_fail(name != NULL);
  g_return_if_fail(glide_is_ui_thread());
  project_index_add_variable(self->index, package, name, doc);
  project_changed(self);
}

const gchar *project_get_variable(Project *self, const gchar *name) {
  g_return_val_if_fail(self != NULL, NULL);
  g_return_val_if_fail(name != NULL, NULL);
  return project_index_get_variable(self->index, name);
}

gchar **project_get_variable_names(Project *self, const gchar *package,
    guint *length) {
  g_return_val_if_fail(self != NULL, NULL);
  return project_index_get_variable_names(self->index, package, length);
}

Package *project_get_package(Project *self, const gchar *name) {
  g_return_val_if_fail(self != NULL, NULL);
  g_return_val_if_fail(name != NULL, NULL);
  return project_index_get_package(self->index, name);
}

gchar **project_get_package_names(Project *self, guint *length) {
  g_return_val_if_fail(self != NULL, NULL);
  return project_index_get_package_names(self->index, length);
}

void project_set_asdf(Project *self, Asdf *asdf) {
  g_return_if_fail(self != NULL);
  g_return_if_fail(glide_is_ui_thread());
  LOG(1, "project_set_asdf %p", asdf);
  if (self->asdf)
    g_object_unref(self->asdf);
  self->asdf = asdf ? g_object_ref(asdf) : NULL;
}

Asdf *project_get_asdf(Project *self) {
  g_return_val_if_fail(self != NULL, NULL);
  return self->asdf;
}

const gchar *project_get_path(Project *self) {
  g_return_val_if_fail(self != NULL, NULL);
  return self->path;
}

void project_set_path(Project *self, const gchar *path) {
  g_return_if_fail(self != NULL);
  g_return_if_fail(glide_is_ui_thread());
  g_free(self->path);
  self->path = path ? g_strdup(path) : NULL;
}

void project_clear(Project *self) {
  g_return_if_fail(self != NULL);
  g_return_if_fail(glide_is_ui_thread());
  LOG(1, "project_clear");
  project_index_clear(self->index);
  if (self->documents) {
    for (guint i = 0; i < self->documents->len; i++) {
      Document *document = g_ptr_array_index(self->documents, i);
      project_document_removed(self, document);
    }
    g_ptr_array_set_size(self->documents, 0);
  }
  Asdf *asdf = asdf_new();
  project_set_asdf(self, asdf);
  g_object_unref(asdf);
  if (self->repl) {
    project_repl_request_package(self->repl, "COMMON-LISP");
    project_repl_request_package(self->repl, "COMMON-LISP-USER");
  }
  project_changed(self);
}

void project_document_changed(Project *self, Document *document) {
  g_return_if_fail(self != NULL);
  g_return_if_fail(document != NULL);
  g_return_if_fail(glide_is_ui_thread());
  LOG(1, "project_document_changed path=%s", document_get_path(document));

  project_index_remove_document(self->index, document);

  const Node *ast = document_get_ast(document);
  if (ast) {
    analyse_ast(self, (Node*)ast);
    project_index_walk(self->index, ast);
  }
  project_changed(self);
}

Project *project_ref(Project *self) {
  g_return_val_if_fail(self != NULL, NULL);
  g_atomic_int_inc(&self->refcnt);
  return self;
}

void project_unref(Project *self) {
  if (!self)
    return;
  if (g_atomic_int_dec_and_test(&self->refcnt))
    project_free(self);
}

void project_document_loaded(Project *self, Document *document) {
  g_return_if_fail(self != NULL);
  g_return_if_fail(document != NULL);
  g_return_if_fail(glide_is_ui_thread());
  project_emit_event(self, PROJECT_CHANGE_EVENT_DOCUMENT_LOADED, document);
  project_changed(self);
}

void project_document_removed(Project *self, Document *document) {
  g_return_if_fail(self != NULL);
  g_return_if_fail(document != NULL);
  g_return_if_fail(glide_is_ui_thread());
  project_emit_event(self, PROJECT_CHANGE_EVENT_DOCUMENT_REMOVED, document);
}

void project_changed(Project *self) {
  g_return_if_fail(self != NULL);
  g_return_if_fail(glide_is_ui_thread());
  LOG(1, "project_changed");
  project_emit_event(self, PROJECT_CHANGE_EVENT_CHANGED, NULL);
}

ProjectRepl *project_get_repl(Project *self) {
  g_return_val_if_fail(self != NULL, NULL);
  return self->repl;
}

guint project_add_event_cb(Project *self, ProjectEventCb cb, gpointer user_data) {
  g_return_val_if_fail(self != NULL, 0);
  g_return_val_if_fail(cb != NULL, 0);
  g_return_val_if_fail(glide_is_ui_thread(), 0);

  ProjectEventHandler *handler = g_new0(ProjectEventHandler, 1);
  handler->callback = cb;
  handler->user_data = user_data;
  handler->id = self->next_event_handler_id++;
  g_ptr_array_add(self->event_handlers, handler);
  return handler->id;
}

void project_remove_event_cb(Project *self, guint handler_id) {
  g_return_if_fail(self != NULL);
  g_return_if_fail(glide_is_ui_thread());
  if (!handler_id)
    return;

  for (guint i = 0; i < self->event_handlers->len; i++) {
    ProjectEventHandler *handler = g_ptr_array_index(self->event_handlers, i);
    if (handler && handler->id == handler_id) {
      g_ptr_array_remove_index(self->event_handlers, i);
      return;
    }
  }
}

static void project_emit_event(Project *self, ProjectChangeEventType type, Document *document) {
  g_return_if_fail(self != NULL);
  g_return_if_fail(glide_is_ui_thread());
  if (!self->event_handlers || !self->event_handlers->len)
    return;
  ProjectChangeEvent event = { type, document };
  for (guint i = 0; i < self->event_handlers->len; i++) {
    ProjectEventHandler *handler = g_ptr_array_index(self->event_handlers, i);
    if (!handler || !handler->callback)
      continue;
    handler->callback(self, &event, handler->user_data);
  }
}

