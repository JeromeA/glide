#ifndef SWANK_SESSION_H
#define SWANK_SESSION_H

#include <glib.h>
#include "interaction.h" // For Interaction struct, used in eval

// Initializes the global Swank session state.
// It will internally set itself up as the callback for the global Swank process.
void swank_session_init_globals();

// Evaluates an expression using the global Swank session.
// The Interaction object should be allocated by the caller and will be managed (and eventually freed)
// by the SwankSession logic or through explicit cleanup if needed.
void swank_session_global_eval(Interaction *interaction);

// Cleans up global Swank session resources (e.g., hash table).
void swank_session_cleanup_globals();

// The on_message callback is internal and registered with the global swank_process.

// InteractionsView is updated directly by new functions.

#endif /* SWANK_SESSION_H */
