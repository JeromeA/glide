#ifndef __CUSTOM_SOURCE_VIEW_H__
#define __CUSTOM_SOURCE_VIEW_H__

#include <gtksourceview/gtksource.h> /* Correct global include for GtkSourceView */

G_BEGIN_DECLS

#define CUSTOM_TYPE_SOURCE_VIEW (custom_source_view_get_type())
G_DECLARE_DERIVABLE_TYPE(CustomSourceView, custom_source_view, CUSTOM, SOURCE_VIEW, GtkSourceView)

struct _CustomSourceViewClass
{
    GtkSourceViewClass parent_class;

    /* Future virtual methods can be added here */

    gpointer padding[12]; /* For future expansion */
};

CustomSourceView *custom_source_view_new(void);

/* Potentially:
CustomSourceView *custom_source_view_new_with_buffer(GtkSourceBuffer *buffer);
void custom_source_view_set_language(CustomSourceView *csv, const gchar *lang_name);
*/

G_END_DECLS

#endif /* __CUSTOM_SOURCE_VIEW_H__ */
