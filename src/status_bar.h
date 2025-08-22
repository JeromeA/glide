#ifndef STATUS_BAR_H
#define STATUS_BAR_H

#include <gtk/gtk.h>

G_BEGIN_DECLS

#define STATUS_BAR_TYPE (status_bar_get_type())
G_DECLARE_FINAL_TYPE(StatusBar, status_bar, GLIDE, STATUS_BAR, GtkBox)

StatusBar *status_bar_new(void);
guint status_bar_publish(StatusBar *self, const gchar *status);
void status_bar_update(StatusBar *self, guint id, const gchar *status);
void status_bar_unpublish(StatusBar *self, guint id);

G_END_DECLS

#endif /* STATUS_BAR_H */
