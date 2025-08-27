# BUGS

This document lists past bugs, their symptoms, and how they were fixed. The goal is to detect patterns and avoid similar
mistakes in the future. A typical entry includes what the goal was, what went wrong, and how it was fixed.

## Crash while clearing the project at startup

The application crashed on startup because `lisp_source_view_dispose` explicitly unreferenced its `GtkSourceView` child
widget. The `GtkScrolledWindow` parent already disposes of its children, so unreferencing the view again left GTK trying
to dispose an already finalized object, triggering the GLib critical `instance with invalid (NULL) class pointer`.

## Crash when restoring last file at startup

The application crashed when restoring the previously opened file at startup. 
`app_activate` tried to scroll the view by casting a `LispSourceView` to 
`GtkTextView`. Since `LispSourceView` is a `GtkScrolledWindow` containing the
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
verbose debugging information during normal evaluations. The evaluation code now
invokes `:emacs-rex` with the debug flag set to `nil`, keeping swank quiet.
