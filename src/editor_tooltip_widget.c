#include "editor_tooltip_widget.h"

#include "util.h"

struct _EditorTooltipWidget
{
  GtkBox parent_instance;

  GtkWidget *error_box;
  GtkWidget *error_label;
  GtkWidget *separator;
  GtkWidget *doc_box;
  GtkWidget *doc_label;
};

struct _EditorTooltipWidgetClass
{
  GtkBoxClass parent_class;
};

G_DEFINE_TYPE (EditorTooltipWidget, editor_tooltip_widget, GTK_TYPE_BOX)

static void editor_tooltip_widget_init_css (void);

static void
editor_tooltip_widget_init (EditorTooltipWidget *self)
{
  gtk_orientable_set_orientation (GTK_ORIENTABLE (self), GTK_ORIENTATION_VERTICAL);
  gtk_box_set_spacing (GTK_BOX (self), 0);

  editor_tooltip_widget_init_css ();

  GtkStyleContext *context = gtk_widget_get_style_context (GTK_WIDGET (self));
  gtk_style_context_add_class (context, "editor-tooltip");

  self->error_box = gtk_box_new (GTK_ORIENTATION_VERTICAL, 0);
  GtkStyleContext *error_context = gtk_widget_get_style_context (self->error_box);
  gtk_style_context_add_class (error_context, "editor-tooltip-error");
  gtk_widget_set_hexpand (self->error_box, TRUE);

  self->error_label = gtk_label_new (NULL);
  gtk_label_set_line_wrap (GTK_LABEL (self->error_label), TRUE);
  gtk_label_set_xalign (GTK_LABEL (self->error_label), 0.0);
  gtk_container_add (GTK_CONTAINER (self->error_box), self->error_label);
  gtk_widget_show (self->error_label);

  gtk_box_pack_start (GTK_BOX (self), self->error_box, TRUE, TRUE, 0);
  gtk_widget_set_visible (self->error_box, FALSE);

  self->separator = gtk_separator_new (GTK_ORIENTATION_HORIZONTAL);
  GtkStyleContext *separator_context = gtk_widget_get_style_context (self->separator);
  gtk_style_context_add_class (separator_context, "editor-tooltip-separator");
  gtk_widget_set_hexpand (self->separator, TRUE);
  gtk_box_pack_start (GTK_BOX (self), self->separator, TRUE, TRUE, 0);
  gtk_widget_set_visible (self->separator, FALSE);

  self->doc_box = gtk_box_new (GTK_ORIENTATION_VERTICAL, 0);
  GtkStyleContext *doc_context = gtk_widget_get_style_context (self->doc_box);
  gtk_style_context_add_class (doc_context, "editor-tooltip-doc");
  gtk_widget_set_hexpand (self->doc_box, TRUE);

  self->doc_label = gtk_label_new (NULL);
  gtk_label_set_line_wrap (GTK_LABEL (self->doc_label), TRUE);
  gtk_label_set_xalign (GTK_LABEL (self->doc_label), 0.0);
  gtk_container_add (GTK_CONTAINER (self->doc_box), self->doc_label);
  gtk_widget_show (self->doc_label);

  gtk_box_pack_start (GTK_BOX (self), self->doc_box, TRUE, TRUE, 0);
  gtk_widget_set_visible (self->doc_box, FALSE);
}

static void
editor_tooltip_widget_class_init (EditorTooltipWidgetClass *klass)
{
  (void) klass;
}

EditorTooltipWidget *
editor_tooltip_widget_new (void)
{
  return g_object_new (EDITOR_TYPE_TOOLTIP_WIDGET, NULL);
}

static void
editor_tooltip_widget_clear_section (GtkWidget *box, GtkWidget *label)
{
  if (!box || !label)
    return;
  gtk_label_set_text (GTK_LABEL (label), "");
  gtk_widget_set_visible (box, FALSE);
}

gboolean
editor_tooltip_widget_set_content (EditorTooltipWidget *self,
    const gchar *error_markup, const gchar *doc_markup)
{
  g_return_val_if_fail (EDITOR_IS_TOOLTIP_WIDGET (self), FALSE);

  gboolean show_error = (error_markup && *error_markup);
  gboolean show_doc = (doc_markup && *doc_markup);

  if (show_error)
    gtk_label_set_markup (GTK_LABEL (self->error_label), error_markup);
  else
    editor_tooltip_widget_clear_section (self->error_box, self->error_label);

  if (show_doc)
    gtk_label_set_markup (GTK_LABEL (self->doc_label), doc_markup);
  else
    editor_tooltip_widget_clear_section (self->doc_box, self->doc_label);

  gtk_widget_set_visible (self->error_box, show_error);
  gtk_widget_set_visible (self->doc_box, show_doc);
  gtk_widget_set_visible (self->separator, show_error && show_doc);

  if (show_error || show_doc)
    gtk_widget_show_all (GTK_WIDGET (self));

  return show_error || show_doc;
}

static void
editor_tooltip_widget_init_css (void)
{
  static gboolean css_loaded = FALSE;
  if (css_loaded)
    return;

  const gchar *css =
      ".editor-tooltip {"
      "  background-color: transparent;"
      "  padding: 0;"
      "  margin: 0;"
      "}"
      ".editor-tooltip-error {"
      "  background-color: #f3f3f3;"
      "  padding: 6px 8px;"
      "}"
      ".editor-tooltip-doc {"
      "  background-color: #ffffff;"
      "  padding: 6px 8px;"
      "}"
      ".editor-tooltip-separator {"
      "  background-color: #a1a1a1;"
      "  min-height: 1px;"
      "  margin: 0;"
      "}";

  GtkCssProvider *provider = gtk_css_provider_new ();
  GError *error = NULL;
  gtk_css_provider_load_from_data (provider, css, -1, &error);
  if (error) {
    LOG (1, "EditorTooltipWidget.init_css: css error: %s", error->message);
    g_error_free (error);
  }

  GdkScreen *screen = gdk_screen_get_default ();
  if (screen)
    gtk_style_context_add_provider_for_screen (screen,
        GTK_STYLE_PROVIDER (provider),
        GTK_STYLE_PROVIDER_PRIORITY_APPLICATION);
  g_object_unref (provider);

  css_loaded = TRUE;
}
