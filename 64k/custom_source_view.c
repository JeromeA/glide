#include "includes.h" /* Common include for the 64k project */
#include "custom_source_view.h"

/*
The G_DEFINE_TYPE macro and G_DECLARE_DERIVABLE_TYPE in the header
handle the creation of the instance structure (_CustomSourceView)
and the class structure (_CustomSourceViewClass).

We only need to define instance members if we add new ones beyond
what GtkSourceView provides. For now, we don't have any custom
members in the instance struct beyond the parent.
If we were to add them, they'd be declared in custom_source_view.h
within the _CustomSourceView struct if it were manually defined, or
conceptually added by the macro system.

For example, if we needed specific members:
struct _CustomSourceViewPrivate {
    GtkSourceGutterRenderer *my_custom_gutter;
    gboolean some_flag;
};
G_DEFINE_TYPE_WITH_PRIVATE(CustomSourceView, custom_source_view, GTK_TYPE_SOURCE_VIEW)
...and then in _init:
self->priv = custom_source_view_get_instance_private(self);
But for now, no private members are strictly needed for the current plan.
The language_manager and language example members are also removed as they are
local to the _init function for now.
*/

G_DEFINE_TYPE(CustomSourceView, custom_source_view, GTK_TYPE_SOURCE_VIEW)

static void
custom_source_view_init(CustomSourceView *self)
{
    /* Initialize instance members here */
    // self->language_manager and self->language are not strictly needed
    // if we only set the language on the buffer at creation and don't
    // change it later through CustomSourceView specific API.
    // For now, let's not add them to the struct.

    GtkSourceLanguageManager *lm = gtk_source_language_manager_get_default();
    GtkSourceLanguage *lang = gtk_source_language_manager_get_language(lm, "commonlisp");

    GtkSourceBuffer *buffer = gtk_source_buffer_new_with_language(lang);
    // The GtkSourceView (parent class) takes its own reference to the buffer.
    gtk_text_view_set_buffer(GTK_TEXT_VIEW(self), GTK_TEXT_BUFFER(buffer));
    // We can unref the buffer now as the view owns it.
    g_object_unref(buffer);

    // Set other default properties for CustomSourceView if any
    gtk_source_view_set_show_line_numbers(GTK_SOURCE_VIEW(self), TRUE);
    // Example:
    // gtk_source_view_set_auto_indent(GTK_SOURCE_VIEW(self), TRUE);
    // gtk_source_view_set_highlight_current_line(GTK_SOURCE_VIEW(self), TRUE);
}

static void
custom_source_view_class_init(CustomSourceViewClass *klass)
{
    GObjectClass *object_class = G_OBJECT_CLASS(klass);
    // GtkWidgetClass *widget_class = GTK_WIDGET_CLASS(klass);
    // GtkTextViewClass *text_view_class = GTK_TEXT_VIEW_CLASS(klass);
    // GtkSourceViewClass *source_view_class = GTK_SOURCE_VIEW_CLASS(klass);

    /* Connect signal handlers or override virtual methods here */
}

CustomSourceView *
custom_source_view_new(void)
{
    // According to the plan, this will later create the buffer internally.
    // For now, it just creates the view. The buffer will be set in app.c
    // during the initial integration, then refactored into here.
    return g_object_new(CUSTOM_TYPE_SOURCE_VIEW, NULL);
}

/*
// Example of a constructor that takes a buffer, if we decide to go that way initially
CustomSourceView *
custom_source_view_new_with_buffer(GtkSourceBuffer *buffer)
{
    g_return_val_if_fail(GTK_IS_SOURCE_BUFFER(buffer), NULL);
    return g_object_new(CUSTOM_TYPE_SOURCE_VIEW,
                        "buffer", buffer,
                        NULL);
}

// Example of setting language if not done via buffer at construction
void
custom_source_view_set_language(CustomSourceView *csv, const gchar *lang_name)
{
    g_return_if_fail(CUSTOM_IS_SOURCE_VIEW(csv));
    g_return_if_fail(lang_name != NULL);

    GtkSourceBuffer *buffer = gtk_source_view_get_buffer(GTK_SOURCE_VIEW(csv));
    if (!buffer) {
        buffer = gtk_source_buffer_new(NULL); // Or handle error
        gtk_text_view_set_buffer(GTK_TEXT_VIEW(csv), GTK_TEXT_BUFFER(buffer));
        g_object_unref(buffer);
    }

    GtkSourceLanguageManager *lm = gtk_source_language_manager_get_default();
    GtkSourceLanguage *lang = gtk_source_language_manager_get_language(lm, lang_name);
    gtk_source_buffer_set_language(buffer, lang);
}
*/
