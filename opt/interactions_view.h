#ifndef INTERACTIONS_VIEW_H
#define INTERACTIONS_VIEW_H

#include <gtk/gtk.h>
// #include "swank_session.h" // SwankSession is now global, no longer passed as GObject
#include "interaction.h" // For Interaction struct

#define INTERACTIONS_VIEW_TYPE (interactions_view_get_type())
G_DECLARE_FINAL_TYPE(InteractionsView, interactions_view, GLIDE, INTERACTIONS_VIEW, GtkBox)

// Constructor no longer takes SwankSession*
InteractionsView *interactions_view_new();

// New public functions to be called by RealSwankSession (via global InteractionsView pointer)
void interactions_view_add_interaction(InteractionsView *iv, Interaction *interaction);
void interactions_view_update_interaction(InteractionsView *iv, Interaction *interaction);

#endif /* INTERACTIONS_VIEW_H */
