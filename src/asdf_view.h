#pragma once

#include <gtk/gtk.h>
#include "asdf.h"

G_BEGIN_DECLS

#define ASDF_VIEW_TYPE (asdf_view_get_type())
G_DECLARE_FINAL_TYPE(AsdfView, asdf_view, ASDF, VIEW, GtkTreeView)

GtkWidget *asdf_view_new(Asdf *asdf);

G_END_DECLS
