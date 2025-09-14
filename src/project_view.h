#pragma once

#include <gtk/gtk.h>
#include "asdf.h"

typedef struct _App App;

G_BEGIN_DECLS

#define PROJECT_VIEW_TYPE (project_view_get_type())
G_DECLARE_FINAL_TYPE(ProjectView, project_view, PROJECT, VIEW, GtkTreeView)

typedef enum {
  PROJECT_VIEW_COL_ICON,
  PROJECT_VIEW_COL_TEXT,
  PROJECT_VIEW_COL_KIND,
  PROJECT_VIEW_COL_OBJECT,
  PROJECT_VIEW_N_COLS
} ProjectViewColumn;

typedef enum {
  PROJECT_VIEW_KIND_ROOT,
  PROJECT_VIEW_KIND_SRC_FOLDER,
  PROJECT_VIEW_KIND_SRC,
  PROJECT_VIEW_KIND_PACKAGES,
  PROJECT_VIEW_KIND_PACKAGE,
  PROJECT_VIEW_KIND_FUNCTION,
  PROJECT_VIEW_KIND_VARIABLE
} ProjectViewKind;
GtkWidget *project_view_new(Asdf *asdf, App *app);
void project_view_select_file(ProjectView *self, const gchar *file);

G_END_DECLS
