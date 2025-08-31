# BUGS

This document lists past bugs, their symptoms, and how they were fixed. The goal is to detect patterns and avoid similar
mistakes in the future. A typical entry includes what the goal was, what went wrong, and how it was fixed.

## Crash while clearing the project at startup

The application crashed on startup because `editor_dispose` explicitly unreferenced its `GtkSourceView` child
widget. The `GtkScrolledWindow` parent already disposes of its children, so unreferencing the view again left GTK trying
to dispose an already finalized object, triggering the GLib critical `instance with invalid (NULL) class pointer`.

## Crash when restoring last file at startup

The application crashed when restoring the previously opened file at startup. 
`app_activate` tried to scroll the view by casting an `Editor` to
`GtkTextView`. Since `Editor` is a `GtkScrolledWindow` containing the
actual `GtkSourceView`, the invalid cast triggered a GLib critical and
terminated the program.

## ASDF viewer no longer selected files

Clicking a filename in the ASDF viewer stopped switching the notebook to that
file. The viewer's model uses a parent node labelled "src" for components, but
the selection handler still expected "components" and ignored selections. The
handler now checks for the correct "src" label so selecting a file in the
viewer updates the notebook.

## ASDF components ignored when using :file

ASDF systems that declared components with the keyword `:file` were ignored.
`node_get_name` returned the full symbol including its package prefix, so
comparisons against `file` failed. `node_get_name` now strips package
delimiters and always returns the symbol name uppercased. The ASDF parser was
updated to compare against upper case names, so `:file` and `file` components
are handled identically.

## Extend selection stuck on atoms

`Ctrl-w` extended the selection to the current symbol but further presses did
not expand to enclosing forms when invoked in the middle of an atom. The AST
contains intermediate nodes whose parents share the same range, so the
selection algorithm climbed only one level and returned the same bounds. The
search now skips parents with identical ranges and therefore reaches the
correct enclosing expression. Debug logging was also added to help diagnose
future issues.

## Crash when evaluating from the Run menu

Selecting "Run > Eval toplevel" triggered a GLib critical and terminated the
program. The "activate" signal for the menu item invoked `on_evaluate` with the
menu item as the first argument, but the handler expected only an `App*`. The
function now uses the standard `GtkWidget *, gpointer` signature and verifies
the `App` instance before evaluating the current form.

## Eval toplevel always picked first expression

Using the "Run > Eval toplevel" action evaluated the first form in the buffer
regardless of the cursor position. The search for the surrounding form scanned
backward for parentheses and stopped at the buffer start when invoked at the
beginning of a line. Evaluation now asks `Editor` for the enclosing
top‑level range, which is shared with the selection expansion logic, so the
correct expression is sent to swank.

## eval-and-capture evaluated raw strings

`eval-and-capture` expected a parsed form but the server sent it expressions as
strings, leading to evaluation failures. The function now reads the string into
an s-expression before evaluating it.

## Session waited for newline after prompt

SBCL's prompt is not terminated by a newline, but `RealReplProcess` only
checked complete lines during startup. The state machine therefore stalled on
"* " waiting for a newline that never arrived, leaving the server unstarted.
Startup handling now inspects the partial buffer for prompts and clears it once
consumed, so Glide no longer requires newline‑terminated prompts.

## Window height increased on each launch

The application grew taller by around 100 pixels every time it started. The
saved window size included the window manager decorations, but the restored
size interpreted the value as the client area, so the extra decoration height
was added on each run. The size allocation handler now records the interior
window size via `gtk_window_get_size`, so the saved height matches the restored
height.

## Interaction view could not scroll

Long evaluation sessions pushed earlier interactions off screen because
`InteractionsView` was a plain `GtkBox` without scrollbars. The widget now
embeds its rows in a `GtkScrolledWindow`, so previous interactions remain
accessible.

## Interactions view truncated without scrollbars

The interactions list was added directly to the `GtkScrolledWindow` without a
`GtkViewport`, so the scrollbars never appeared when the view was shorter than
its contents. Wrapping the box in a viewport allows the window to scroll.

## Interactions view hid new interactions without scrollbars

When evaluation produced many interactions, new rows were appended below the
visible area. The window neither grew nor displayed a scrollbar, so the only
way to see the new interactions was to drag the pane divider to enlarge the
view. The scrolled window now disables natural height propagation so the window
scrolls when content exceeds the available space.

## Interactions view still did not scroll

Even after disabling natural height propagation, the interactions list still
expanded with its contents and never triggered the scrollbars. The surrounding
viewport did not advertise that it could shrink, so the scrolled window kept
resizing to fit the growing box. Marking the viewport as vertically expandable
prevents this, letting the view scroll when there are many interactions.

## Interactions view packed without resizing

The interactions pane was inserted into the main window's `GtkPaned` with the
`resize` flag set to `FALSE`. As interactions accumulated, the pane grew to fit
its contents instead of allowing the embedded scrolled window to show
scrollbars. Packing the pane with `resize` set to `TRUE` and marking the widget
as vertically expandable keeps the pane at a fixed height and enables scrolling
when needed.

## Interactions view shrank with its viewport

Dragging the pane divider to reduce the interactions view did not reveal
scrollbars. The `GtkViewport` inside the view was marked as vertically
expandable, so it shrank along with the scrolled window and never exceeded its
allocation. Removing the expansion flag lets the viewport keep its natural
height, allowing the scrollbars to appear when the pane is too small.

## Interactions view left updates off screen

New interactions or additional output could appear below the visible area while
the scroll position stayed unchanged. The view now scrolls to keep the most
recent interaction in sight.

## Interactions view still scrolled unreliably

Scrolling to the latest interaction relied on an idle callback that sometimes
ran before GTK completed its resize, leaving the view above the newest content.
The interactions box now scrolls in response to its size allocating growing, so
the view reliably follows new output without race conditions.

## Stdout and stderr output did not update interactions

REPL sessions appended stdout and stderr text to the interaction but did not
invoke the update callback, so the UI missed incremental output. The session now
notifies listeners when stdout or stderr arrives.

## Debug logging split across lines

Messages containing newlines caused `g_debug_160` to emit multi-line log entries, making logs harder to read.
The helper now escapes newline characters so each debug message stays on a single line.

## ReplProcess deadlocked on incomplete messages

`ReplProcess` locked its mutex while accumulating output but returned early when the buffer lacked a complete
sexp. The missing unlock left the mutex held, so the next chunk blocked waiting for the lock and tests hung.
`on_proc_out` now releases the mutex before returning when output is incomplete, allowing processing to
continue.

## Interactions view accessed freed text

`InteractionsView` filled its widgets with pointers to strings stored in `Interaction`. The session thread
could update an interaction concurrently, freeing or replacing those strings while the UI still held references.
`Interaction` now guards its fields with a mutex, and both the session and view lock around any access,
copying text as needed to keep pointers valid.

## Package info name mismatch

`Project` asked the server for `glide:package-definition`, but the Lisp code exported `package-info`
under a different name. The missing function prevented fetching package metadata. Renaming the file and
function to `package-definition` synchronized the API and restored package lookup.
