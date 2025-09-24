#ifndef EDITOR_TOOLTIP_WINDOW_H
#define EDITOR_TOOLTIP_WINDOW_H

#include <gtk/gtk.h>

G_BEGIN_DECLS

typedef struct _EditorTooltipWindow EditorTooltipWindow;
typedef struct _EditorTooltipWindowClass EditorTooltipWindowClass;

#define EDITOR_TYPE_TOOLTIP_WINDOW (editor_tooltip_window_get_type ())
#define EDITOR_TOOLTIP_WINDOW(obj) \
    (G_TYPE_CHECK_INSTANCE_CAST ((obj), EDITOR_TYPE_TOOLTIP_WINDOW, EditorTooltipWindow))
#define EDITOR_IS_TOOLTIP_WINDOW(obj) \
    (G_TYPE_CHECK_INSTANCE_TYPE ((obj), EDITOR_TYPE_TOOLTIP_WINDOW))

gboolean               editor_tooltip_window_set_content (EditorTooltipWindow *self,
    const gchar *error_markup, const gchar *doc_markup);
gboolean               editor_tooltip_window_has_content (EditorTooltipWindow *self);
EditorTooltipWindow   *editor_tooltip_window_new          (void);
GType                  editor_tooltip_window_get_type     (void);

G_END_DECLS

#endif /* EDITOR_TOOLTIP_WINDOW_H */
