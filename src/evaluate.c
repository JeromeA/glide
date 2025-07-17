/* evaluate.c
 *
 * Implements evaluation of the code found on the current line in the
 * GtkSourceView buffer.  The text is forwarded to the Swank backend for
 * remote execution.
 */

#include "swank_session.h"
#include "interaction.h"
#include "evaluate.h"

#include <gtk/gtk.h>
#include <gtksourceview/gtksource.h>

/* ------------------------------------------------------------------------- */
/* Callback triggered when the user requests evaluation of the current line. */
/* ------------------------------------------------------------------------- */
void
on_evaluate(App *self)
{
  g_debug("Evaluate.on_evaluate");
  GtkSourceBuffer *source_buffer = app_get_source_buffer(self);

  /* 1. Locate the iterator at the caret (insert mark). */
  GtkTextMark *insert_mark = gtk_text_buffer_get_insert(GTK_TEXT_BUFFER(source_buffer));
  GtkTextIter cursor_iter;
  gtk_text_buffer_get_iter_at_mark(GTK_TEXT_BUFFER(source_buffer), &cursor_iter, insert_mark);

  /* 2. Create iterators for the start and end of the current line. */
  GtkTextIter line_start = cursor_iter;
  gtk_text_iter_set_line_offset(&line_start, 0);           /* column 0 */

  GtkTextIter line_end = line_start;
  gtk_text_iter_forward_to_line_end(&line_end);            /* move to EOL */

  /* 3. Extract the line‟s contents (excluding the trailing newline). */
  gchar *expr = gtk_text_buffer_get_text(GTK_TEXT_BUFFER(source_buffer),
      &line_start,
      &line_end,
      FALSE);          /* no hidden chars */

  if (expr == NULL || *expr == '\0') {
    g_debug("Evaluate.on_evaluate: nothing to evaluate on the current line");
    g_free(expr);
    return;
  }

  /* 4. Send the expression to SWANK for remote execution. */
  SwankSession *swank = app_get_swank(self);
  if (swank) {
    Interaction *interaction = g_new0(Interaction, 1);
    interaction_init(interaction, expr);
    swank_session_eval(swank, interaction);
    /* The interaction will be cleared when the session returns the result. */
  }

  g_free(expr);
}

