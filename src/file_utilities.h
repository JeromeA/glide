#pragma once

#include <glib.h>

static inline gchar *ensure_lisp_extension(const gchar *path) {
  if (g_str_has_suffix(path, ".lisp"))
    return g_strdup(path);
  return g_strconcat(path, ".lisp", NULL);
}

