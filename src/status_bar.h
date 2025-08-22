#ifndef STATUS_BAR_H
#define STATUS_BAR_H

#include <gtk/gtk.h>
#include "status_service.h"

G_BEGIN_DECLS

#define STATUS_BAR_TYPE (status_bar_get_type())
G_DECLARE_FINAL_TYPE(StatusBar, status_bar, GLIDE, STATUS_BAR, GtkBox)

StatusBar *status_bar_new(StatusService *service);

G_END_DECLS

#endif /* STATUS_BAR_H */
