#ifndef INTERACTIONS_VIEW_H
#define INTERACTIONS_VIEW_H

#include <gtk/gtk.h>
#include "swank_session.h"

#define INTERACTIONS_VIEW_TYPE (interactions_view_get_type())
G_DECLARE_FINAL_TYPE(InteractionsView, interactions_view, GLIDE, INTERACTIONS_VIEW, GtkBox)

InteractionsView *interactions_view_new(SwankSession *session);

#endif /* INTERACTIONS_VIEW_H */
