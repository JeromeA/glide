#include "includes.h"
#include "glide.h"
#include "file_open.h"
#include "file_save.h"
#include "preferences_dialog.h"
#include "evaluate.h"

/* === Instance structure ================================================= */
struct _Glide
{
  GtkApplication  parent_instance;

  /* UI pointers we want to reuse */
  GtkWidget      *window;
  GtkSourceBuffer*buffer;
  gchar          *filename;   /* current file path or NULL */
};

static gboolean
on_key_press (GtkWidget *,
    GdkEventKey *event,
    gpointer     user_data)   /* actually Glide* */
{
  Glide *self = (Glide *) user_data;

  if ((event->keyval == GDK_KEY_Return) &&
      (event->state  & GDK_MOD1_MASK))      /* Alt+Enter */
  {
    on_evaluate(self);
    return TRUE;                  /* stop further propagation */
  }
  return FALSE;
}


/* === GObject boiler-plate ============================================== */
G_DEFINE_TYPE(Glide, glide, GTK_TYPE_APPLICATION)

/* ---  class_init ------------------------------------------------------- */
static void
glide_activate (GApplication *app)
{
  Glide *self = GLIDE_APP(app);

  /*--------------------------------------------------------------*
   *  Build the UI (this is almost a verbatim move from app.c)    *
   *--------------------------------------------------------------*/
  self->window = gtk_application_window_new (GTK_APPLICATION (app));
  gtk_window_set_default_size (GTK_WINDOW (self->window), 800, 600);
  g_signal_connect (self->window, "delete-event", G_CALLBACK (gtk_window_close), NULL);

  /* Scrolled source-view */
  GtkWidget *scrolled = gtk_scrolled_window_new (NULL, NULL);
  gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (scrolled), GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);

  /* Source buffer + language */
  GtkSourceLanguageManager *lm = gtk_source_language_manager_get_default ();
  GtkSourceLanguage *lang = gtk_source_language_manager_get_language (lm, "commonlisp");
  self->buffer = gtk_source_buffer_new_with_language (lang);

  GtkWidget *view = gtk_source_view_new_with_buffer (self->buffer);
  gtk_source_view_set_show_line_numbers (GTK_SOURCE_VIEW (view), TRUE);
  gtk_container_add (GTK_CONTAINER (scrolled), view);

  /* Catch Alt+Enter in the view */
  g_signal_connect (view, "key-press-event", G_CALLBACK (on_key_press), self);

  /* Menu bar ------------------------------------------------------ */
  GtkWidget *menu_bar      = gtk_menu_bar_new ();
  GtkWidget *file_menu     = gtk_menu_new ();
  GtkWidget *file_item     = gtk_menu_item_new_with_label ("File");
  GtkWidget *open_item     = gtk_menu_item_new_with_label ("Open…");
  GtkWidget *save_item     = gtk_menu_item_new_with_label ("Save");
  GtkWidget *saveas_item   = gtk_menu_item_new_with_label ("Save as…");
  GtkWidget *pref_item     = gtk_menu_item_new_with_label ("Preferences…");
  GtkWidget *quit_item     = gtk_menu_item_new_with_label ("Quit");

  gtk_menu_item_set_submenu (GTK_MENU_ITEM (file_item), file_menu);
  gtk_menu_shell_append (GTK_MENU_SHELL (file_menu), open_item);
  gtk_menu_shell_append (GTK_MENU_SHELL (file_menu), save_item);
  gtk_menu_shell_append (GTK_MENU_SHELL (file_menu), saveas_item);
  gtk_menu_shell_append (GTK_MENU_SHELL (file_menu), pref_item);
  gtk_menu_shell_append (GTK_MENU_SHELL (file_menu), quit_item);
  gtk_menu_shell_append (GTK_MENU_SHELL (menu_bar), file_item);

  /* Pass the buffer to file-handlers */
  g_signal_connect (open_item,   "activate", G_CALLBACK (file_open),   self);
  g_signal_connect (save_item,   "activate", G_CALLBACK (file_save),   self);
  g_signal_connect (saveas_item, "activate", G_CALLBACK (file_saveas), self);

  /* Preferences need a window */
  g_signal_connect (pref_item, "activate", G_CALLBACK (on_preferences), self);
  g_signal_connect (quit_item, "activate", G_CALLBACK (g_application_quit), app);

  /* Vertical box with menu + view */
  GtkWidget *vbox = gtk_box_new (GTK_ORIENTATION_VERTICAL, 0);
  gtk_box_pack_start (GTK_BOX (vbox), menu_bar,     FALSE, FALSE, 0);
  gtk_box_pack_start (GTK_BOX (vbox), scrolled,     TRUE,  TRUE,  0);
  gtk_container_add   (GTK_CONTAINER (self->window), vbox);

  gtk_widget_show_all (self->window);
}

static void
glide_startup (GApplication *app)
{
  /* Chain up first */
  G_APPLICATION_CLASS (glide_parent_class)->startup (app);

  /* Anything that has to happen *before* activate goes here */
  relocate ();   /* from the original code */
}

static void
glide_dispose (GObject *object)
{
  Glide *self = GLIDE_APP(object);

  g_clear_pointer (&self->filename, g_free);
  G_OBJECT_CLASS (glide_parent_class)->dispose (object);
}

static void
glide_class_init (GlideClass *klass)
{
  GApplicationClass *app_class = G_APPLICATION_CLASS (klass);
  GObjectClass      *obj_class = G_OBJECT_CLASS (klass);

  app_class->startup  = glide_startup;
  app_class->activate = glide_activate;
  obj_class->dispose  = glide_dispose;
}

static void
glide_init (Glide *self)
{
  /* Everything that needs only the *instance* goes here */
  self->filename = NULL;
}

STATIC Glide *
glide_new (void)
{
  return g_object_new (GLIDE_TYPE,
      /* GtkApplication properties */
      "application-id",    "org.example.Glide",
      "flags",             G_APPLICATION_HANDLES_OPEN,
      NULL);
}

STATIC GtkSourceBuffer *
glide_get_source_buffer (Glide *self)
{
  g_return_val_if_fail (GLIDE_APP (self), NULL);
  return self->buffer;
}

STATIC const gchar *
glide_get_filename (Glide *self)
{
  g_return_val_if_fail (GLIDE_APP (self), NULL);
  return self->filename;
}

STATIC void
glide_set_filename (Glide *self, const gchar *new_filename)
{
  g_return_if_fail (GLIDE_APP (self));
  gchar *dup = new_filename ? g_strdup (new_filename) : NULL;
  g_free (self->filename);
  self->filename = dup;

  /* If you later register a “filename” property, notify here:
     g_object_notify_by_pspec (G_OBJECT (self), obj_props[PROP_FILENAME]); */
}
