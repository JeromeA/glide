#include "interactions_view.h"

struct _InteractionsView {
  GtkBox parent_instance;
};

G_DEFINE_TYPE(InteractionsView, interactions_view, GTK_TYPE_BOX)

static void
interactions_view_class_init(InteractionsViewClass *klass)
{
  g_debug("InteractionsView.class_init");
}

static void
interactions_view_init(InteractionsView *self)
{
  g_debug("InteractionsView.init");
  gtk_orientable_set_orientation(GTK_ORIENTABLE(self), GTK_ORIENTATION_VERTICAL);
}

InteractionsView *
interactions_view_new(void)
{
  g_debug("InteractionsView.new");
  return g_object_new(INTERACTIONS_VIEW_TYPE, NULL);
}
