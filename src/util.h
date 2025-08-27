#pragma once

#include <glib.h>

static inline void g_debug_160(const char *string, const char *msg)
{
  if (strlen(msg) > 160)
    g_debug("%s%.160s...", string, msg);
  else
    g_debug("%s%s", string, msg);
}

