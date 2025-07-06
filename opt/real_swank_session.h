#ifndef REAL_SWANK_SESSION_H
#define REAL_SWANK_SESSION_H

#include <glib.h>
#include "interaction.h" // For Interaction struct, used in eval

// swank_session.h has been removed.
// swank_process.h has been removed (RealSwankSession will use global swank process functions).

// Initializes the global Swank session state.
// It will internally set itself up as the callback for the global Swank process.
void real_swank_session_init_globals();

// Evaluates an expression using the global Swank session.
// The Interaction object should be allocated by the caller and will be managed (and eventually freed)
// by the RealSwankSession logic or through explicit cleanup if needed.
void real_swank_session_global_eval(Interaction *interaction);

// Cleans up global Swank session resources (e.g., hash table).
void real_swank_session_cleanup_globals();

// The on_message callback is now internal to real_swank_session.c and registered
// with the global real_swank_process. It does not need to be in the public API.

// Signals for interaction added/updated are removed.
// InteractionsView will need a new way to get updates.

#endif /* REAL_SWANK_SESSION_H */
