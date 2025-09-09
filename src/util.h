#pragma once

#include <glib.h>

static inline gboolean glide_is_ui_thread(void)
{
  return g_main_context_is_owner(g_main_context_default());
}

static inline void g_debug_long(const char *string, const char *msg)
{
  char *escaped = g_strescape(msg, NULL);
  size_t len = strlen(escaped);
  if (len > 160)
    g_log(G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "%s%.80s...%s", string,
        escaped, escaped + len - 80);
  else
    g_log(G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "%s%s", string, escaped);
  g_free(escaped);
}

#define VERBOSITY 1

#define LOG(level, ...) \
    do { if (VERBOSITY >= (level)) \
      g_log(G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, __VA_ARGS__); } while (0)

#define LOG_LONG(level, str, data) \
    do { if (VERBOSITY >= (level)) g_debug_long(str, data); } while (0)
