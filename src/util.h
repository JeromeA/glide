#pragma once

#include <glib.h>

static inline void g_debug_80(const char *string, const char *msg)
{
  if (strlen(msg) > 80)
    g_debug("%s%.80s...", string, msg);
  else
    g_debug("%s%s", string, msg);
}

