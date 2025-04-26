#ifndef FIND_EXECUTABLES_H
#define FIND_EXECUTABLES_H

#include <glib.h>

#ifndef STATIC
#define STATIC
#endif

/**
 * Returns a newly allocated GPtrArray (with g_free as the element free func) containing
 * the full paths to all the Lisp executables found in the system.
 * The caller must free the array with g_ptr_array_free(found, TRUE).
 */
STATIC GPtrArray *find_lisp_executables(void);

#endif /* FIND_EXECUTABLES_H */
