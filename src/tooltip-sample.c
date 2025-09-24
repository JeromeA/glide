#include <gtk/gtk.h>

static gboolean
on_query_tooltip(GtkWidget *widget, gint x, gint y, gboolean keyboard_mode,
                 GtkTooltip *tooltip, gpointer user_data)
{
  // We are using our own tooltip window; do NOT use gtk_tooltip_set_custom() here.
  GtkWindow *tw = gtk_widget_get_tooltip_window(widget);  // returns GtkWindow*
  if (!tw) return FALSE; // shouldn't happen once we've set a custom window

  // The window is a GtkBin -> get its first child (our box)
  GtkWidget *box = gtk_bin_get_child(GTK_BIN(tw));

  // Clear any previous content
  if (box) {
    GList *children = gtk_container_get_children(GTK_CONTAINER(box));
    for (GList *l = children; l; l = l->next)
      gtk_container_remove(GTK_CONTAINER(box), GTK_WIDGET(l->data));
    g_list_free(children);
  }

  // Add your content
  GtkWidget *label = gtk_label_new("Single content.");
  gtk_container_add(GTK_CONTAINER(box), label);
  gtk_widget_show_all(GTK_WIDGET(box));

  return TRUE;
}

int main(int argc, char **argv)
{
  gtk_init(&argc, &argv);

  const gchar *css =
    ".tooltip {"
    "  background-color: #fff;"
    "}";
  GtkCssProvider *prov = gtk_css_provider_new();
  gtk_css_provider_load_from_data(prov, css, -1, NULL);
  gtk_style_context_add_provider_for_screen(gdk_screen_get_default(),
      GTK_STYLE_PROVIDER(prov), GTK_STYLE_PROVIDER_PRIORITY_APPLICATION);
  g_object_unref(prov);

  GtkWidget *win = gtk_window_new(GTK_WINDOW_TOPLEVEL);
  gtk_window_set_default_size(GTK_WINDOW(win), 200, 100);
  g_signal_connect(win, "destroy", G_CALLBACK(gtk_main_quit), NULL);

  GtkWidget *label = gtk_label_new("hover me");
  gtk_widget_set_halign(label, GTK_ALIGN_CENTER);
  gtk_widget_set_valign(label, GTK_ALIGN_CENTER);
  gtk_container_add(GTK_CONTAINER(win), label);

  // Create our own tooltip window (no 6px margins)
  GtkWidget *tipwin = gtk_window_new(GTK_WINDOW_POPUP);
  gtk_window_set_type_hint(GTK_WINDOW(tipwin), GDK_WINDOW_TYPE_HINT_TOOLTIP);

  // Give it the usual tooltip classes so CSS applies
  GtkStyleContext *ctx = gtk_widget_get_style_context(tipwin);
  gtk_style_context_add_class(ctx, "tooltip");

  GtkWidget *box = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 0);
  gtk_container_add(GTK_CONTAINER(tipwin), box);
  gtk_widget_show(box);

  // Use our window for this widget’s tooltip
  gtk_widget_set_tooltip_window(label, GTK_WINDOW(tipwin));
  gtk_widget_set_has_tooltip(label, TRUE);
  g_signal_connect(label, "query-tooltip", G_CALLBACK(on_query_tooltip), NULL);

  gtk_widget_show_all(win);
  gtk_main();
  return 0;
}

