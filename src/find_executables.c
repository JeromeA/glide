#include <gtk/gtk.h>  /* pulls in glib.h, g_file_test, g_build_filename, GPtrArray */

static const gchar *LISP_NAMES[] = {
  "sbcl",
  "clisp",
};
static const guint N_LISP_NAMES = G_N_ELEMENTS(LISP_NAMES);

static const gchar *SEARCH_DIRS[] = {
  "/usr/bin",
  "/usr/local/bin",
};
static const guint N_SEARCH_DIRS = G_N_ELEMENTS(SEARCH_DIRS);
 
#ifndef STATIC
#define STATIC
#endif

/**
 * find_executables:
 * @names:    array of executable basenames to look for
 * @n_names:  number of names in @names
 * @dirs:     array of directory paths to search
 * @n_dirs:   number of dirs in @dirs
 *
 * Scans each directory in @dirs for each basename in @names,
 * testing if it's both present and executable.  Returns a newly
 * allocated GPtrArray (with g_free as the element free func)
 * containing full paths (gchar*) to every matching executable.
 *
 * The caller must later do:
 *      g_ptr_array_free(found, TRUE);
 */
STATIC GPtrArray *
find_executables(const gchar * const names[],
                 guint               n_names,
                 const gchar * const dirs[],
                 guint               n_dirs)
{
  GPtrArray *found = g_ptr_array_new_with_free_func(g_free);

  for (guint i = 0; i < n_dirs; i++) {
    for (guint j = 0; j < n_names; j++) {
      gchar *full_path = g_build_filename(dirs[i], names[j], NULL);

      /* Check existence AND executable bit */
      if (g_file_test(full_path, G_FILE_TEST_EXISTS) &&
          g_file_test(full_path, G_FILE_TEST_IS_EXECUTABLE)) {
        /* Keep it */
        g_ptr_array_add(found, full_path);
      } else {
        /* Not executable or doesn’t exist: free it */
        g_free(full_path);
      }
    }
  }

  return found;
}

STATIC GPtrArray *
find_lisp_executables(void)
{
  return find_executables(
    LISP_NAMES, N_LISP_NAMES,
    SEARCH_DIRS, N_SEARCH_DIRS
  );
}

