#ifndef EDITOR_TOOLTIP_WIDGET_H
#define EDITOR_TOOLTIP_WIDGET_H

#include <gtk/gtk.h>

G_BEGIN_DECLS

typedef struct _EditorTooltipWidget EditorTooltipWidget;
typedef struct _EditorTooltipWidgetClass EditorTooltipWidgetClass;

#define EDITOR_TYPE_TOOLTIP_WIDGET (editor_tooltip_widget_get_type())
#define EDITOR_TOOLTIP_WIDGET(obj) \
    (G_TYPE_CHECK_INSTANCE_CAST ((obj), EDITOR_TYPE_TOOLTIP_WIDGET, EditorTooltipWidget))
#define EDITOR_IS_TOOLTIP_WIDGET(obj) \
    (G_TYPE_CHECK_INSTANCE_TYPE ((obj), EDITOR_TYPE_TOOLTIP_WIDGET))

gboolean               editor_tooltip_widget_set_content (EditorTooltipWidget *self,
    const gchar *error_markup, const gchar *doc_markup);
gboolean               editor_tooltip_widget_has_content (EditorTooltipWidget *self);
EditorTooltipWidget   *editor_tooltip_widget_new          (void);
GType       editor_tooltip_widget_get_type     (void);

G_END_DECLS

#endif /* EDITOR_TOOLTIP_WIDGET_H */
