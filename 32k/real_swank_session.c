#include "real_swank_session.h"
#include <string.h>
#include "util.h"

struct _RealSwankSession {
  GObject parent_instance;
  SwankProcess *proc;
  gboolean started;
};

static void real_swank_session_eval(SwankSession *session, const gchar *expr);

static void
real_swank_session_swank_session_iface_init(SwankSessionInterface *iface)
{
  g_debug("RealSwankSession.swank_session_iface_init");
  iface->eval = real_swank_session_eval;
}

G_DEFINE_TYPE_WITH_CODE(RealSwankSession, real_swank_session, G_TYPE_OBJECT,
    G_IMPLEMENT_INTERFACE(SWANK_SESSION_TYPE,
        real_swank_session_swank_session_iface_init))

static void
real_swank_session_finalize(GObject *obj)
{
  g_debug("RealSwankSession.finalize");
  RealSwankSession *self = GLIDE_REAL_SWANK_SESSION(obj);
  if (self->proc)
    g_object_unref(self->proc);
  G_OBJECT_CLASS(real_swank_session_parent_class)->finalize(obj);
}

static void
real_swank_session_class_init(RealSwankSessionClass *klass)
{
  g_debug("RealSwankSession.class_init");
  GObjectClass *obj = G_OBJECT_CLASS(klass);
  obj->finalize = real_swank_session_finalize;
}

static void
real_swank_session_init(RealSwankSession *self)
{
  g_debug("RealSwankSession.init");
  self->proc = NULL;
  self->started = FALSE;
}

SwankSession *
real_swank_session_new(SwankProcess *proc)
{
  g_debug("RealSwankSession.new");
  RealSwankSession *self = g_object_new(REAL_SWANK_SESSION_TYPE, NULL);
  self->proc = proc ? g_object_ref(proc) : NULL;
  self->started = FALSE;
  if (self->proc)
    swank_process_set_message_cb(self->proc, real_swank_session_on_message, self);
  return GLIDE_SWANK_SESSION(self);
}

static gchar *
escape_string(const char *str)
{
  g_debug("RealSwankSession.escape_string input:%s", str);
  GString *out = g_string_new(NULL);
  for (const char *p = str; *p; p++) {
    switch (*p) {
      case '\\': g_string_append(out, "\\\\"); break;
      case '"':  g_string_append(out, "\\\""); break;
      default:    g_string_append_c(out, *p);
    }
  }
  gchar *ret = g_string_free(out, FALSE);
  g_debug("RealSwankSession.escape_string output:%s", ret);
  return ret;
}

static void
real_swank_session_eval(SwankSession *session, const gchar *expr)
{
  g_debug("RealSwankSession.eval %s", expr);
  RealSwankSession *self = GLIDE_REAL_SWANK_SESSION(session);
  if (!self->proc)
    return;
  if (!self->started) {
    swank_process_start(self->proc);
    self->started = TRUE;
  }
  gchar *escaped = escape_string(expr);
  gchar *rpc = g_strdup_printf("(:emacs-rex (swank:eval-and-grab-output \"%s\") \"COMMON-LISP-USER\" t 1)", escaped);
  GString *payload = g_string_new(rpc);
  g_free(escaped);
  g_free(rpc);
  swank_process_send(self->proc, payload);
  g_string_free(payload, TRUE);
}

static inline gboolean ascii_isspace(char c)
{
  switch ((unsigned char)c) {
    case ' ': case '\t': case '\n': case '\r': case '\f': case '\v':
      return TRUE;
    default:
      return FALSE;
  }
}

static gchar *
next_token(const char **p)
{
  const char *s = *p;
  while (ascii_isspace(*s)) s++;
  const char *start = s;
  if (*s == '(') {
    int depth = 1;
    gboolean in_str = FALSE;
    gboolean esc = FALSE;
    s++;
    for (; *s && depth > 0; s++) {
      char c = *s;
      if (esc) {
        esc = FALSE;
      } else if (c == '\\') {
        esc = TRUE;
      } else if (c == '"') {
        in_str = !in_str;
      } else if (!in_str) {
        if (c == '(')
          depth++;
        else if (c == ')')
          depth--;
      }
    }
    *p = s;
    while (ascii_isspace(**p)) (*p)++;
    return g_strndup(start, s - start);
  } else if (*s == '"') {
    gboolean esc = FALSE;
    s++;
    for (; *s; s++) {
      char c = *s;
      if (esc)
        esc = FALSE;
      else if (c == '\\')
        esc = TRUE;
      else if (c == '"') {
        s++;
        break;
      }
    }
    *p = s;
    while (ascii_isspace(**p)) (*p)++;
    return g_strndup(start, s - start);
  } else {
    for (; *s && !ascii_isspace(*s) && *s != ')'; s++)
      ;
    *p = s;
    while (ascii_isspace(**p)) (*p)++;
    return g_strndup(start, s - start);
  }
}

static void
on_return(const gchar *value, const gchar *token)
{
  g_debug("RealSwankSession.on_return %s %s", value, token);
}

static void
on_new_features(const gchar *value)
{
  g_debug_40("RealSwankSession.on_new_features ", value);
}

static void
on_indentation_update(const gchar *value)
{
  g_debug_40("RealSwankSession.on_indentation_update ", value);
}

void
real_swank_session_on_message(GString *msg, gpointer /*user_data*/)
{
  g_debug_40("RealSwankSession.on_message ", msg->str);

  const char *p = msg->str;
  if (g_str_has_prefix(p, "(:return ")) {
    p += strlen("(:return ");
    gchar *arg1 = next_token(&p);
    gchar *arg2 = next_token(&p);
    on_return(arg1, arg2);
    g_free(arg1);
    g_free(arg2);
  } else if (g_str_has_prefix(p, "(:new-features ")) {
    p += strlen("(:new-features ");
    gchar *arg = next_token(&p);
    on_new_features(arg);
    g_free(arg);
  } else if (g_str_has_prefix(p, "(:indentation-update ")) {
    p += strlen("(:indentation-update ");
    gchar *arg = next_token(&p);
    on_indentation_update(arg);
    g_free(arg);
  }
}

