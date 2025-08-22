#include "status_bar.h"

struct _StatusBar
{
  GtkBox parent_instance;
  GtkWidget *label;
  StatusService *service;
};

G_DEFINE_TYPE(StatusBar, status_bar, GTK_TYPE_BOX)

static void
on_changed(const gchar *text, gpointer data)
{
  StatusBar *self = GLIDE_STATUS_BAR(data);
  gtk_label_set_text(GTK_LABEL(self->label), text);
}

static void
status_bar_init(StatusBar *self)
{
  gtk_orientable_set_orientation(GTK_ORIENTABLE(self), GTK_ORIENTATION_HORIZONTAL);
  self->label = gtk_label_new("Ready");
  gtk_box_pack_start(GTK_BOX(self), self->label, FALSE, FALSE, 0);
  self->service = NULL;
}

static void
status_bar_finalize(GObject *object)
{
  StatusBar *self = GLIDE_STATUS_BAR(object);
  if (self->service)
    status_service_set_callback(self->service, NULL, NULL);
  G_OBJECT_CLASS(status_bar_parent_class)->finalize(object);
}

static void
status_bar_class_init(StatusBarClass *klass)
{
  GObjectClass *object_class = G_OBJECT_CLASS(klass);
  object_class->finalize = status_bar_finalize;
}

StatusBar *
status_bar_new(StatusService *service)
{
  StatusBar *self = g_object_new(STATUS_BAR_TYPE, NULL);
  self->service = service;
  status_service_set_callback(service, on_changed, self);
  gtk_label_set_text(GTK_LABEL(self->label), status_service_get(service));
  return self;
}
