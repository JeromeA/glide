/* evaluate.c
 *
 * Implements evaluation of the top-level form at the caret in the
 * GtkSourceView buffer.  The text is forwarded to the Swank backend for
 * remote execution.
 */

#include "swank_session.h"
#include "interaction.h"
#include "evaluate.h"
#include "lisp_source_view.h"

#include <gtk/gtk.h>
#include <gtksourceview/gtksource.h>

/* ------------------------------------------------------------------------- */
/* Callback triggered when the user requests evaluation of the current form. */
/* ------------------------------------------------------------------------- */
void
on_evaluate(App *self)
{
  g_debug("Evaluate.on_evaluate");
  GtkSourceBuffer *source_buffer =
      lisp_source_view_get_buffer(app_get_source_view(self));
  GtkTextMark *insert_mark = gtk_text_buffer_get_insert(GTK_TEXT_BUFFER(source_buffer));
  GtkTextIter cursor_iter;
  gtk_text_buffer_get_iter_at_mark(GTK_TEXT_BUFFER(source_buffer), &cursor_iter, insert_mark);

  GtkTextIter start = cursor_iter;
  gint depth = 0;
  while (gtk_text_iter_backward_char(&start)) {
    gunichar ch = gtk_text_iter_get_char(&start);
    if (ch == ')')
      depth++;
    else if (ch == '(') {
      if (depth == 0)
        break;
      depth--;
    }
  }

  GtkTextIter end = start;
  depth = 0;
  do {
    gunichar ch = gtk_text_iter_get_char(&end);
    if (ch == '(')
      depth++;
    else if (ch == ')') {
      depth--;
      if (depth == 0) {
        gtk_text_iter_forward_char(&end);
        break;
      }
    }
  } while (gtk_text_iter_forward_char(&end));

  gchar *expr = gtk_text_buffer_get_text(GTK_TEXT_BUFFER(source_buffer),
      &start,
      &end,
      FALSE);

  if (expr == NULL || *expr == '\0') {
    g_debug("Evaluate.on_evaluate: nothing to evaluate");
    g_free(expr);
    return;
  }

  SwankSession *swank = app_get_swank(self);
  if (swank) {
    Interaction *interaction = g_new0(Interaction, 1);
    interaction_init(interaction, expr);
    swank_session_eval(swank, interaction);
  }

  g_free(expr);
}

