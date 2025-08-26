#pragma once

#include <gtk/gtk.h>
#include "asdf.h"

typedef struct _App App;

G_BEGIN_DECLS

#define ASDF_VIEW_TYPE (asdf_view_get_type())
G_DECLARE_FINAL_TYPE(AsdfView, asdf_view, ASDF, VIEW, GtkTreeView)

typedef enum {
  ASDF_VIEW_COL_TEXT,
  ASDF_VIEW_COL_KIND,
  ASDF_VIEW_COL_OBJECT,
  ASDF_VIEW_N_COLS
} AsdfViewColumn;

typedef enum {
  ASDF_VIEW_KIND_ROOT,
  ASDF_VIEW_KIND_SRC,
  ASDF_VIEW_KIND_COMPONENT,
  ASDF_VIEW_KIND_LIBRARIES,
  ASDF_VIEW_KIND_LIBRARY
} AsdfViewKind;

GtkWidget *asdf_view_new(Asdf *asdf, App *app);
void asdf_view_select_file(AsdfView *self, const gchar *file);

G_END_DECLS
