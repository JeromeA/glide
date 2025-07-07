#ifndef REAL_SWANK_SESSION_H
#define REAL_SWANK_SESSION_H

#include <glib.h>
#include "interaction.h" // For Interaction struct, used in eval

// Initializes the global Swank session state.
// It will internally set itself up as the callback for the global Swank process.
void real_swank_session_init_globals();

// Evaluates an expression using the global Swank session.
// The Interaction object should be allocated by the caller and will be managed (and eventually freed)
// by the RealSwankSession logic or through explicit cleanup if needed.
void real_swank_session_global_eval(Interaction *interaction);

// Cleans up global Swank session resources (e.g., hash table).
void real_swank_session_cleanup_globals();

// The on_message callback is internal and registered with the global real_swank_process.

// InteractionsView is updated directly by new functions.

#endif /* REAL_SWANK_SESSION_H */
