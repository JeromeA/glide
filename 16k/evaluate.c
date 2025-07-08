/* evaluate.c
 *
 * Implements evaluation of the code found on the current line in the
 * GtkSourceView buffer.  The text is forwarded to the Swank backend for
 * remote execution.
 */

#include "evaluate.h" // Include self header
#include "interaction.h"

#include <gtk/gtk.h>
#include <gtksourceview/gtksource.h>

// These variables are defined in main.c
extern GtkSourceBuffer *buffer;
#include "swank_session.h"


/* ------------------------------------------------------------------------- */
/* Callback triggered when the user requests evaluation of the current line. */
/* ------------------------------------------------------------------------- */
void
on_evaluate()
{
  GtkSourceBuffer *source_buffer = buffer;
  if (!source_buffer) {
    g_warning("Evaluate.on_evaluate: buffer is NULL");
    return;
  }

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
    g_free(expr);
    return;
  }

  /* 4. Send the expression to SWANK for remote execution. */
  // Directly call the eval function
  Interaction *interaction = g_new0(Interaction, 1);
  interaction_init(interaction, expr); // expr is owned by interaction now
  swank_session_eval(interaction);
  // Interaction will be managed (and eventually freed) by RealSwankSession logic.


  // Looking at interaction.c, interaction_init(i,e) { i->expression = g_strdup(e); }
  // So, expr from gtk_text_buffer_get_text should be freed here.
  g_free(expr);
}

