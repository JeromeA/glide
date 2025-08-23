#pragma once

#include <glib.h>

static inline void g_debug_40(const char *string, const char *msg)
{
  if (strlen(msg) > 40)
    g_debug("%s%.40s...", string, msg);
  else
    g_debug("%s%s", string, msg);
}

