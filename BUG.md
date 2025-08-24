The application crashed on startup because `lisp_source_view_dispose` explicitly
unreferenced its `GtkSourceView` child widget. The `GtkScrolledWindow` parent
already disposes of its children, so unreferencing the view again left GTK
trying to dispose an already finalized object, triggering the GLib critical
`instance with invalid (NULL) class pointer`.
