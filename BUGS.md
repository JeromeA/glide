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

## Swank debug mode left enabled

Evaluating expressions kept `swank:*swank-debug-p*` set to `t`, so swank printed
verbose debugging information during normal evaluations. The swank process now
evaluates `(setf swank:*global-debugger* nil)` before starting the server,
keeping swank quiet.

## Eval toplevel always picked first expression

Using the "Run > Eval toplevel" action evaluated the first form in the buffer
regardless of the cursor position. The search for the surrounding form scanned
backward for parentheses and stopped at the buffer start when invoked at the
beginning of a line. Evaluation now asks `Editor` for the enclosing
top‑level range, which is shared with the selection expansion logic, so the
correct expression is sent to swank.

## Debugger still activated during evaluations

Disabling swank's global debugger was not enough to prevent the debugger from
grabbing control. The swank process now let-binds `*debugger-hook*` to `nil`
when evaluating forms and sends them in the `CL-USER` package, so evaluations
no longer drop into the debugger unexpectedly.

## Debugger hook bound outside evaluated form

`*debugger-hook*` was `let`-bound around `swank:eval-and-grab-output`, leaving
the evaluated form itself unprotected. Errors inside the form still invoked the
debugger. The binding is now wrapped around the expression passed to swank so
evaluations run with the debugger hook disabled.

## glide-eval evaluated raw strings

`glide-eval` expected a parsed form but the server sent it expressions as
strings, leading to evaluation failures. The function now reads the string into
an s-expression before evaluating it.

## Interaction results sometimes missing

Evaluations occasionally displayed only the entered expression with no result.
`InteractionsView` created rows only when the "added" signal arrived, but the
backend could return a result before the row was created. The "updated"
handler now ensures the row exists and updates it, so results are always shown.
Debug logging was added to `RealReplSession` to help trace message handling.

## Evaluation result hidden by startup prompt

Selecting an expression sometimes displayed only the form with no result.
SBCL prints a `*` prompt and its `NIL` result while loading Glide. Those
startup lines arrived just before the first evaluation and confused the
session, so the interaction row missed the real result. `RealReplProcess`
now waits for SBCL's initial prompt, ignores the `NIL` and second prompt from
`(require :glide)`, and only then starts the server, so sessions receive the
actual evaluation results.

## Session failed to start with trailing prompt space

SBCL prints its prompt as `* ` with a trailing space. `RealReplProcess` looked
for a line containing only `*`, so the startup handshake never progressed and
the server was not started. `RealReplProcess` now trims whitespace from
startup lines and logs each step of the handshake, ensuring sessions start
reliably and the progress is visible in debug logs.

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

## Debugger hook disabled debugging

Evaluations wrapped with `(let ((*debugger-hook* nil)) ...)` prevented the
debugger from handling errors. Removing the binding restores normal debugger
behavior during evaluations.

## Interactions view truncated without scrollbars

The interactions list was added directly to the `GtkScrolledWindow` without a
`GtkViewport`, so the scrollbars never appeared when the view was shorter than
its contents. Wrapping the box in a viewport allows the window to scroll.
