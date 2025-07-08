
#ifndef EVALUATE_H
#define EVALUATE_H

#include <gtksourceview/gtksource.h> // For GtkSourceBuffer used by on_evaluate if it were to take it

// on_evaluate is implemented in evaluate.c, which handles its own dependencies.
// This header only needs to declare the function.
void
on_evaluate();

#endif // EVALUATE_H
