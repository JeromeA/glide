#include "actions.h"
#include "file_save.h"
#include "file_open.h"
#include "file_rename.h"
#include "evaluate.h"
#include "lisp_parser_view.h"
#include "project_file.h"
#include "lisp_source_notebook.h"
#include "lisp_source_view.h"

static gboolean app_maybe_save_all(App *self);

void
on_show_parser(App *self)
{
  GtkWidget *win = gtk_window_new(GTK_WINDOW_TOPLEVEL);
  gtk_window_set_title(GTK_WINDOW(win), "Parser View");
  LispSourceNotebook *notebook = app_get_notebook(self);
  LispSourceView *current = lisp_source_notebook_get_current_view(notebook);
  ProjectFile *file = lisp_source_view_get_file(current);
  GtkWidget *view = lisp_parser_view_new(file);
  GtkWidget *scrolled = gtk_scrolled_window_new(NULL, NULL);
  gtk_container_add(GTK_CONTAINER(scrolled), view);
  gtk_container_add(GTK_CONTAINER(win), scrolled);
  gtk_widget_show_all(win);
}

void
on_extend_selection(GtkWidget * /*item*/, gpointer data)
{
  App *self = GLIDE_APP(data);
  LispSourceNotebook *notebook = app_get_notebook(self);
  LispSourceView *view = lisp_source_notebook_get_current_view(notebook);
  if (view)
    lisp_source_view_extend_selection(view);
}

void
on_shrink_selection(GtkWidget * /*item*/, gpointer data)
{
  App *self = GLIDE_APP(data);
  LispSourceNotebook *notebook = app_get_notebook(self);
  LispSourceView *view = lisp_source_notebook_get_current_view(notebook);
  if (view)
    lisp_source_view_shrink_selection(view);
}

void
on_save_all(GtkWidget * /*item*/, gpointer data)
{
  App *self = GLIDE_APP(data);
  file_save_all(app_get_project(self));
}

void
on_close_project(GtkWidget * /*item*/, gpointer data)
{
  g_debug("Actions.on_close_project");
  app_close_project(GLIDE_APP(data), TRUE);
}

void
on_quit_menu(GtkWidget * /*item*/, gpointer data)
{
  g_debug("Actions.on_quit_menu");
  app_on_quit(GLIDE_APP(data));
}

gboolean
on_quit_delete_event(GtkWidget * /*widget*/, GdkEvent * /*event*/, gpointer data)
{
  g_debug("Actions.on_quit_delete_event");
  app_on_quit(GLIDE_APP(data));
  return TRUE;
}

void
on_recent_project(GtkWidget *item, gpointer data)
{
  App *self = GLIDE_APP(data);
  const gchar *path = g_object_get_data(G_OBJECT(item), "project-path");
  if (path)
    file_open_path(self, path);
}

gboolean
on_key_press(GtkWidget * /*widget*/, GdkEventKey *event, gpointer user_data)
{
  App *self = (App *) user_data;

  if ((event->keyval == GDK_KEY_Return) &&
      (event->state & GDK_MOD1_MASK))
  {
    on_evaluate(NULL, self);
    return TRUE;
  }
  if ((event->keyval == GDK_KEY_p || event->keyval == GDK_KEY_P) &&
      (event->state & GDK_MOD1_MASK))
  {
    on_show_parser(self);
    return TRUE;
  }
  if ((event->keyval == GDK_KEY_w || event->keyval == GDK_KEY_W) &&
      ((event->state & (GDK_CONTROL_MASK | GDK_SHIFT_MASK)) ==
       (GDK_CONTROL_MASK | GDK_SHIFT_MASK)))
  {
    LispSourceNotebook *notebook = app_get_notebook(self);
    LispSourceView *view = lisp_source_notebook_get_current_view(notebook);
    if (view)
      lisp_source_view_shrink_selection(view);
    return TRUE;
  }
  if ((event->keyval == GDK_KEY_w || event->keyval == GDK_KEY_W) &&
      ((event->state & (GDK_CONTROL_MASK | GDK_SHIFT_MASK)) ==
       GDK_CONTROL_MASK))
  {
    LispSourceNotebook *notebook = app_get_notebook(self);
    LispSourceView *view = lisp_source_notebook_get_current_view(notebook);
    if (view)
      lisp_source_view_extend_selection(view);
    return TRUE;
  }
  if ((event->keyval == GDK_KEY_F6) &&
      (event->state & GDK_SHIFT_MASK))
  {
    file_rename(NULL, self);
    return TRUE;
  }
  return FALSE;
}

static gboolean
app_maybe_save_all(App *self)
{
  LispSourceNotebook *notebook = app_get_notebook(self);
  if (!notebook)
    return TRUE;
  gint pages = gtk_notebook_get_n_pages(GTK_NOTEBOOK(notebook));
  gboolean modified = FALSE;
  for (gint i = 0; i < pages; i++) {
    GtkWidget *view = gtk_notebook_get_nth_page(GTK_NOTEBOOK(notebook), i);
    GtkTextBuffer *buffer = view ? GTK_TEXT_BUFFER(lisp_source_view_get_buffer(LISP_SOURCE_VIEW(view))) : NULL;
    if (buffer && gtk_text_buffer_get_modified(buffer)) {
      modified = TRUE;
      break;
    }
  }
  if (!modified)
    return TRUE;
  GtkWidget *dialog = gtk_message_dialog_new(NULL, GTK_DIALOG_MODAL,
      GTK_MESSAGE_QUESTION, GTK_BUTTONS_NONE,
      "Save changes to project?");
  gtk_dialog_add_button(GTK_DIALOG(dialog), "_Cancel", GTK_RESPONSE_CANCEL);
  gtk_dialog_add_button(GTK_DIALOG(dialog), "_Discard", GTK_RESPONSE_REJECT);
  gtk_dialog_add_button(GTK_DIALOG(dialog), "_Save", GTK_RESPONSE_ACCEPT);
  gint res = gtk_dialog_run(GTK_DIALOG(dialog));
  gtk_widget_destroy(dialog);
  if (res == GTK_RESPONSE_CANCEL)
    return FALSE;
  if (res == GTK_RESPONSE_ACCEPT)
    file_save_all(app_get_project(self));
  return TRUE;
}

gboolean
app_close_project(App *self, gboolean forget_project)
{
  g_debug("Actions.app_close_project forget=%d", forget_project);
  g_return_val_if_fail(GLIDE_IS_APP(self), FALSE);
  if (!app_maybe_save_all(self))
    return FALSE;
  Project *project = app_get_project(self);
  project_clear(project);
  Preferences *prefs = app_get_preferences(self);
  if (prefs && forget_project) {
    preferences_set_project_file(prefs, NULL);
    preferences_set_last_file(prefs, NULL);
    preferences_set_cursor_position(prefs, 0);
  }
  app_update_asdf_view(self);
  return TRUE;
}
