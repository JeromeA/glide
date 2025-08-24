The application crashed when restoring the previously opened file at startup. 
`app_activate` tried to scroll the view by casting a `LispSourceView` to 
`GtkTextView`. Since `LispSourceView` is a `GtkScrolledWindow` containing the
actual `GtkSourceView`, the invalid cast triggered a GLib critical and
terminated the program.
