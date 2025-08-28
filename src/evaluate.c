/* evaluate.c
 *
 * Implements evaluation of Lisp code in the GtkSourceView buffer.  The text
 * is forwarded to the Glide backend for remote execution.
 */

#include <gtk/gtk.h>
#include <gtksourceview/gtksource.h>

#include "glide_session.h"
#include "interaction.h"
#include "evaluate.h"
#include "editor.h"
#include "app.h"

/* ------------------------------------------------------------------------- */
/* Callback triggered when the user requests evaluation of the current form. */
/* ------------------------------------------------------------------------- */
void
on_evaluate_toplevel(GtkWidget * /*item*/, gpointer data) /* actually App* */
{
  App *self = (App *) data;
  g_debug("Evaluate.on_evaluate_toplevel");
  g_return_if_fail(GLIDE_IS_APP(self));
  Editor *view = app_get_editor(self);
  GtkSourceBuffer *source_buffer = editor_get_buffer(view);
  GtkTextMark *insert_mark =
      gtk_text_buffer_get_insert(GTK_TEXT_BUFFER(source_buffer));
  GtkTextIter cursor_iter;
  gtk_text_buffer_get_iter_at_mark(GTK_TEXT_BUFFER(source_buffer),
      &cursor_iter, insert_mark);
  gsize offset = gtk_text_iter_get_offset(&cursor_iter);

  gsize start_offset;
  gsize end_offset;
  if (!editor_get_toplevel_range(view, offset, &start_offset,
      &end_offset)) {
    g_debug("Evaluate.on_evaluate_toplevel: nothing to evaluate");
    return;
  }

  GtkTextIter start_iter;
  GtkTextIter end_iter;
  gtk_text_buffer_get_iter_at_offset(GTK_TEXT_BUFFER(source_buffer),
      &start_iter, start_offset);
  gtk_text_buffer_get_iter_at_offset(GTK_TEXT_BUFFER(source_buffer),
      &end_iter, end_offset);
  gchar *expr = gtk_text_buffer_get_text(GTK_TEXT_BUFFER(source_buffer),
      &start_iter, &end_iter, FALSE);
  if (!expr || *expr == '\0') {
    g_debug("Evaluate.on_evaluate_toplevel: nothing to evaluate");
    g_free(expr);
    return;
  }

  g_debug("Evaluate.on_evaluate_toplevel expr: %s", expr);

  GlideSession *glide = app_get_glide(self);
  if (glide) {
    Interaction *interaction = g_new0(Interaction, 1);
    interaction_init(interaction, expr);
    glide_session_eval(glide, interaction);
  }

  g_free(expr);
}

/* ------------------------------------------------------------------------- */
/* Callback triggered when the user requests evaluation of the selection.   */
/* ------------------------------------------------------------------------- */
void
on_evaluate_selection(GtkWidget * /*item*/, gpointer data) /* actually App* */
{
  App *self = (App *) data;
  g_debug("Evaluate.on_evaluate_selection");
  g_return_if_fail(GLIDE_IS_APP(self));
  Editor *view = app_get_editor(self);
  GtkSourceBuffer *source_buffer = editor_get_buffer(view);
  GtkTextIter start_iter;
  GtkTextIter end_iter;
  if (!gtk_text_buffer_get_selection_bounds(GTK_TEXT_BUFFER(source_buffer),
      &start_iter, &end_iter)) {
    g_debug("Evaluate.on_evaluate_selection: nothing to evaluate");
    return;
  }

  gchar *expr = gtk_text_buffer_get_text(GTK_TEXT_BUFFER(source_buffer),
      &start_iter, &end_iter, FALSE);
  if (!expr || *expr == '\0') {
    g_debug("Evaluate.on_evaluate_selection: nothing to evaluate");
    g_free(expr);
    return;
  }

  g_debug("Evaluate.on_evaluate_selection expr: %s", expr);

  GlideSession *glide = app_get_glide(self);
  if (glide) {
    Interaction *interaction = g_new0(Interaction, 1);
    interaction_init(interaction, expr);
    glide_session_eval(glide, interaction);
  }

  g_free(expr);
}

