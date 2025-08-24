# BUGS

This document lists past bugs, their symptoms, and how they were fixed. The goal is to detect patterns and avoid similar
mistakes in the future. A typical entry includes what the goal was, what went wrong, and how it was fixed.

## Crash while clearing the project at startup

The application crashed on startup because `lisp_source_view_dispose` explicitly unreferenced its `GtkSourceView` child
widget. The `GtkScrolledWindow` parent already disposes of its children, so unreferencing the view again left GTK trying
to dispose an already finalized object, triggering the GLib critical `instance with invalid (NULL) class pointer`.


