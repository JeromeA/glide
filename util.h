#ifndef GLIDE_UTIL_H
#define GLIDE_UTIL_H

#include <glib.h>

static inline void g_debug_40(const char *string, const char *msg)
{
  if (strlen(msg) > 40)
    g_debug("%s%.40s...", string, msg);
  else
    g_debug("%s%s", string, msg);
}

#endif /* GLIDE_UTIL_H */
