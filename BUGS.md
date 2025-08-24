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
