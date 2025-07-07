
#ifndef EVALUATE_H
#define EVALUATE_H

#include <gtksourceview/gtksource.h> // For GtkSourceBuffer used by on_evaluate_global if it were to take it

// on_evaluate_global is implemented in evaluate.c, which handles its own dependencies.
// This header only needs to declare the function.
void
on_evaluate_global();

#endif // EVALUATE_H
