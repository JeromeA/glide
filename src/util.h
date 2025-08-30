#pragma once

#include <glib.h>

static inline void g_debug_160(const char *string, const char *msg)
{
  char *escaped = g_strescape(msg, NULL);
  size_t len = strlen(escaped);
  if (len > 160)
    g_debug("%s%.80s...%s", string, escaped, escaped + len - 80);
  else
    g_debug("%s%s", string, escaped);
  g_free(escaped);
}

