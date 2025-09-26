#pragma once

#include <glib.h>

static inline const char *utf8_next_char(const char *p)
{
  unsigned char c = (unsigned char)*p;
  if ((c & 0x80) == 0)
    return p + 1;
  if ((c & 0xE0) == 0xC0)
    return p + 2;
  if ((c & 0xF0) == 0xE0)
    return p + 3;
  if ((c & 0xF8) == 0xF0)
    return p + 4;
  /* fallback: treat as single byte to avoid infinite loop */
  return p + 1;
}

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

#define LOG(level, ...) \
    do { if (VERBOSITY >= (level)) \
      g_log(G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, __VA_ARGS__); } while (0)

#define LOG_LONG(level, str, data) \
    do { if (VERBOSITY >= (level)) g_debug_long(str, data); } while (0)
