#pragma once

#include <glib.h>

int get_verbosity(void);

#undef g_debug
#define LOG(level, ...) \
    do { if (get_verbosity() >= (level)) \
      g_log(G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, __VA_ARGS__); } while (0)

#define g_debug(...) LOG(1, __VA_ARGS__)

static inline void g_debug_160(int level, const char *string, const char *msg)
{
  if (get_verbosity() >= level) {
    char *escaped = g_strescape(msg, NULL);
    size_t len = strlen(escaped);
    if (len > 160)
      g_log(G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "%s%.80s...%s", string,
          escaped, escaped + len - 80);
    else
      g_log(G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "%s%s", string, escaped);
    g_free(escaped);
  }
}

