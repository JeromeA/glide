#include "swank_session.h"

#include <string.h>

struct _SwankSession {
  GObject parent_instance;
  SwankProcessImpl *proc;
};

G_DEFINE_TYPE(SwankSession, swank_session, G_TYPE_OBJECT)

static void swank_session_finalize(GObject *obj)
{
  SwankSession *self = GLIDE_SWANK_SESSION(obj);
  if (self->proc)
    swank_process_free(self->proc);
  G_OBJECT_CLASS(swank_session_parent_class)->finalize(obj);
}

static void swank_session_class_init(SwankSessionClass *klass)
{
  GObjectClass *obj = G_OBJECT_CLASS(klass);
  obj->finalize = swank_session_finalize;
}

static void swank_session_init(SwankSession *self)
{
  self->proc = NULL;
}

SwankSession *
swank_session_new(SwankProcessImpl *proc)
{
  SwankSession *self = g_object_new(SWANK_SESSION_TYPE, NULL);
  self->proc = proc;
  return self;
}

static gchar *
escape_string(const char *str)
{
  GString *out = g_string_new(NULL);
  for (const char *p = str; *p; p++) {
    switch (*p) {
      case '\\': g_string_append(out, "\\\\"); break;
      case '"':  g_string_append(out, "\\\""); break;
      default:    g_string_append_c(out, *p);
    }
  }
  return g_string_free(out, FALSE);
}

void
swank_session_eval(SwankSession *self, const gchar *expr)
{
  if (!self->proc)
    return;
  gchar *escaped = escape_string(expr);
  gchar *rpc = g_strdup_printf("(:emacs-rex \"%s\" \"COMMON-LISP-USER\" t 1)", escaped);
  GString *payload = g_string_new(rpc);
  g_free(escaped);
  g_free(rpc);
  swank_process_send(self->proc, payload);
  g_string_free(payload, TRUE);
}
