#pragma once

#include <glib.h>
#include "node.h"
#include "package.h"
#include "function.h"
#include "document.h"

typedef struct _ProjectIndex ProjectIndex;

ProjectIndex *project_index_new(void);
void          project_index_free(ProjectIndex *self);

void          project_index_walk(ProjectIndex *self, const Node *node);
GHashTable   *project_index_get(ProjectIndex *self, StringDesignatorType sd_type);
void          project_index_remove_document(ProjectIndex *self, Document *document);
void          project_index_clear(ProjectIndex *self);

void          project_index_add_package(ProjectIndex *self, Package *package);
Package      *project_index_get_package(ProjectIndex *self, const gchar *name);
gchar       **project_index_get_package_names(ProjectIndex *self, guint *length);

void          project_index_add_function(ProjectIndex *self, Function *function);
Function     *project_index_get_function(ProjectIndex *self, const gchar *name);
gchar       **project_index_get_function_names(ProjectIndex *self,
    const gchar *package, guint *length);

void          project_index_add_variable(ProjectIndex *self, const gchar *package,
    const gchar *name, const gchar *doc);
const gchar  *project_index_get_variable(ProjectIndex *self, const gchar *name);
gchar       **project_index_get_variable_names(ProjectIndex *self,
    const gchar *package, guint *length);

