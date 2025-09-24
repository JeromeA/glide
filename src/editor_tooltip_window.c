#include "editor_tooltip_window.h"

#include "util.h"

struct _EditorTooltipWindow
{
  GtkWindow parent_instance;

  GtkWidget *content_box;
  GtkWidget *error_box;
  GtkWidget *error_label;
  GtkWidget *separator;
  GtkWidget *doc_box;
  GtkWidget *doc_label;

  gboolean has_error;
  gboolean has_doc;

  gchar *error_markup;
  gchar *doc_markup;
};

struct _EditorTooltipWindowClass
{
  GtkWindowClass parent_class;
};

G_DEFINE_TYPE (EditorTooltipWindow, editor_tooltip_window, GTK_TYPE_WINDOW)

static void editor_tooltip_window_init_css (void);

static void
editor_tooltip_window_init (EditorTooltipWindow *self)
{
  self->has_error = FALSE;
  self->has_doc = FALSE;
  self->error_markup = NULL;
  self->doc_markup = NULL;

  editor_tooltip_window_init_css ();

  self->content_box = gtk_box_new (GTK_ORIENTATION_VERTICAL, 0);
  GtkStyleContext *context = gtk_widget_get_style_context (self->content_box);
  gtk_style_context_add_class (context, "editor-tooltip");
  gtk_widget_set_hexpand (self->content_box, TRUE);
  gtk_container_add (GTK_CONTAINER (self), self->content_box);

  self->error_box = gtk_box_new (GTK_ORIENTATION_VERTICAL, 0);
  GtkStyleContext *error_context = gtk_widget_get_style_context (self->error_box);
  gtk_style_context_add_class (error_context, "editor-tooltip-error");
  gtk_widget_set_hexpand (self->error_box, TRUE);

  self->error_label = gtk_label_new (NULL);
  gtk_label_set_line_wrap (GTK_LABEL (self->error_label), TRUE);
  gtk_label_set_xalign (GTK_LABEL (self->error_label), 0.0);
  gtk_container_add (GTK_CONTAINER (self->error_box), self->error_label);
  gtk_widget_show (self->error_label);

  gtk_box_pack_start (GTK_BOX (self->content_box), self->error_box, TRUE, TRUE, 0);
  gtk_widget_set_visible (self->error_box, FALSE);

  self->separator = gtk_separator_new (GTK_ORIENTATION_HORIZONTAL);
  GtkStyleContext *separator_context = gtk_widget_get_style_context (self->separator);
  gtk_style_context_add_class (separator_context, "editor-tooltip-separator");
  gtk_widget_set_hexpand (self->separator, TRUE);
  gtk_box_pack_start (GTK_BOX (self->content_box), self->separator, TRUE, TRUE, 0);
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

  gtk_box_pack_start (GTK_BOX (self->content_box), self->doc_box, TRUE, TRUE, 0);
  gtk_widget_set_visible (self->doc_box, FALSE);
}

static void
editor_tooltip_window_finalize (GObject *object)
{
  EditorTooltipWindow *self = EDITOR_TOOLTIP_WINDOW (object);

  g_clear_pointer (&self->error_markup, g_free);
  g_clear_pointer (&self->doc_markup, g_free);

  G_OBJECT_CLASS (editor_tooltip_window_parent_class)->finalize (object);
}

static void
editor_tooltip_window_class_init (EditorTooltipWindowClass *klass)
{
  GObjectClass *object_class = G_OBJECT_CLASS (klass);
  object_class->finalize = editor_tooltip_window_finalize;
}

static void
editor_tooltip_window_clear_section (GtkWidget *box, GtkWidget *label)
{
  if (!box || !label)
    return;
  gtk_label_set_text (GTK_LABEL (label), "");
  gtk_widget_set_visible (box, FALSE);
}

EditorTooltipWindow *
editor_tooltip_window_new_popup (void)
{
  EditorTooltipWindow *self = g_object_new (EDITOR_TYPE_TOOLTIP_WINDOW,
      "type", GTK_WINDOW_POPUP,
      NULL);

  gtk_window_set_type_hint (GTK_WINDOW (self), GDK_WINDOW_TYPE_HINT_TOOLTIP);
  gtk_window_set_resizable (GTK_WINDOW (self), FALSE);
  gtk_window_set_decorated (GTK_WINDOW (self), FALSE);
  gtk_widget_set_app_paintable (GTK_WIDGET (self), TRUE);

  return self;
}

EditorTooltipWindow *
editor_tooltip_window_new_toplevel (void)
{
  EditorTooltipWindow *self = g_object_new (EDITOR_TYPE_TOOLTIP_WINDOW,
      "type", GTK_WINDOW_TOPLEVEL,
      NULL);

  gtk_window_set_title (GTK_WINDOW (self), "Tooltip");
  gtk_window_set_type_hint (GTK_WINDOW (self), GDK_WINDOW_TYPE_HINT_DIALOG);

  return self;
}

gboolean
editor_tooltip_window_set_content (EditorTooltipWindow *self,
    const gchar *error_markup, const gchar *doc_markup)
{
  g_return_val_if_fail (EDITOR_IS_TOOLTIP_WINDOW (self), FALSE);

  gboolean show_error = (error_markup && *error_markup);
  gboolean show_doc = (doc_markup && *doc_markup);

  if (show_error)
    gtk_label_set_markup (GTK_LABEL (self->error_label), error_markup);
  else
    editor_tooltip_window_clear_section (self->error_box, self->error_label);

  if (show_doc)
    gtk_label_set_markup (GTK_LABEL (self->doc_label), doc_markup);
  else
    editor_tooltip_window_clear_section (self->doc_box, self->doc_label);

  gtk_widget_set_visible (self->error_box, show_error);
  gtk_widget_set_visible (self->doc_box, show_doc);
  gtk_widget_set_visible (self->separator, show_error && show_doc);

  g_clear_pointer (&self->error_markup, g_free);
  g_clear_pointer (&self->doc_markup, g_free);

  self->has_error = show_error;
  self->has_doc = show_doc;

  if (show_error)
    self->error_markup = g_strdup (error_markup);
  if (show_doc)
    self->doc_markup = g_strdup (doc_markup);

  if (show_error || show_doc)
    gtk_widget_show_all (self->content_box);

  return show_error || show_doc;
}

gboolean
editor_tooltip_window_has_content (EditorTooltipWindow *self)
{
  g_return_val_if_fail (EDITOR_IS_TOOLTIP_WINDOW (self), FALSE);
  return self->has_error || self->has_doc;
}

void
editor_tooltip_window_copy_content (EditorTooltipWindow *self,
    EditorTooltipWindow *source)
{
  g_return_if_fail (EDITOR_IS_TOOLTIP_WINDOW (self));
  g_return_if_fail (EDITOR_IS_TOOLTIP_WINDOW (source));

  const gchar *error_markup = source->has_error ? source->error_markup : NULL;
  const gchar *doc_markup = source->has_doc ? source->doc_markup : NULL;

  editor_tooltip_window_set_content (self, error_markup, doc_markup);
}

static void
editor_tooltip_window_init_css (void)
{
  static gboolean css_loaded = FALSE;
  if (css_loaded)
    return;

  const gchar *css =
      "tooltip {"
      "  background-color: #f00;"
      "  padding: 0;"
      "  margin: 0;"
      "  outline: 0;"
      "  border: 0;"
      "}"
      ".editor-tooltip {"
      "  padding: 0;"
      "  margin: 0;"
      "  outline: 0;"
      "  border: 0;"
      "}"
      ".editor-tooltip-error {"
      "  padding: 0;"
      "  margin: 0;"
      "  border: 0;"
      "}"
      ".editor-tooltip-error label {"
      "  background-color: #eee;"
      "}"
      ".editor-tooltip-doc {"
      "  padding: 0;"
      "  margin: 0;"
      "  border: 0;"
      "}"
      ".editor-tooltip-doc label{"
      "  background-color: #ddd;"
      "}"
      ".editor-tooltip-separator {"
      "  background-color: #555;"
      "  min-height: 1px;"
      "  padding: 0;"
      "  margin: 0;"
      "  border: 0;"
      "}"
      ;

  GtkCssProvider *provider = gtk_css_provider_new ();
  GError *error = NULL;
  gtk_css_provider_load_from_data (provider, css, -1, &error);
  if (error) {
    LOG (1, "EditorTooltipWindow.init_css: css error: %s", error->message);
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
