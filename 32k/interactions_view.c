#include "interactions_view.h"
#include "interaction.h"
#include "swank_session.h"

struct _InteractionsView {
  GtkBox parent_instance;
  SwankSession *session;
  gulong handler_id;
};

G_DEFINE_TYPE(InteractionsView, interactions_view, GTK_TYPE_BOX)

static void
interactions_view_finalize(GObject *obj)
{
  g_debug("InteractionsView.finalize");
  InteractionsView *self = GLIDE_INTERACTIONS_VIEW(obj);
  if (self->session && self->handler_id)
    g_signal_handler_disconnect(self->session, self->handler_id);
  if (self->session)
    g_object_unref(self->session);
  G_OBJECT_CLASS(interactions_view_parent_class)->finalize(obj);
}

static void
interactions_view_class_init(InteractionsViewClass *klass)
{
  g_debug("InteractionsView.class_init");
  GObjectClass *obj = G_OBJECT_CLASS(klass);
  obj->finalize = interactions_view_finalize;
}

static void
interactions_view_init(InteractionsView *self)
{
  g_debug("InteractionsView.init");
  gtk_orientable_set_orientation(GTK_ORIENTABLE(self), GTK_ORIENTATION_VERTICAL);
  self->session = NULL;
  self->handler_id = 0;
}

InteractionsView *
interactions_view_new(SwankSession *session)
{
  g_debug("InteractionsView.new");
  g_return_val_if_fail(GLIDE_IS_SWANK_SESSION(session), NULL);
  InteractionsView *self = g_object_new(INTERACTIONS_VIEW_TYPE, NULL);
  self->session = g_object_ref(session);
  self->handler_id = g_signal_connect(self->session, "interaction-added",
      G_CALLBACK(on_interaction_added), self);
  return self;
}

static void
on_interaction_added(SwankSession * /*session*/, Interaction *interaction, gpointer user_data)
{
  InteractionsView *self = GLIDE_INTERACTIONS_VIEW(user_data);
  g_debug("InteractionsView.on_interaction_added %s", interaction->expression);
}

