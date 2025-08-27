#pragma once

#include <gtk/gtk.h>
#include "glide_session.h"

#define INTERACTIONS_VIEW_TYPE (interactions_view_get_type())
G_DECLARE_FINAL_TYPE(InteractionsView, interactions_view, GLIDE, INTERACTIONS_VIEW, GtkBox)

InteractionsView *interactions_view_new(GlideSession *session);

