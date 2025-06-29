#include "real_swank_session.h"
#include <string.h>
#include "util.h"
#include "interaction.h"

struct _RealSwankSession {
  GObject parent_instance;
  SwankProcess *proc;
  gboolean started;
  guint32 next_tag;
  GHashTable *interactions;
};

enum {
  INTERACTION_ADDED,
  INTERACTION_UPDATED,
  SWANK_SESSION_SIGNAL_COUNT
};

static guint real_swank_session_signals[SWANK_SESSION_SIGNAL_COUNT] = { 0 };

static void real_swank_session_eval(SwankSession *session, Interaction *interaction);

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
  if (self->interactions)
    g_hash_table_destroy(self->interactions);
  G_OBJECT_CLASS(real_swank_session_parent_class)->finalize(obj);
}

static void
real_swank_session_class_init(RealSwankSessionClass *klass)
{
  g_debug("RealSwankSession.class_init");
  real_swank_session_signals[INTERACTION_ADDED] = g_signal_new(
      "interaction-added",
      G_TYPE_FROM_CLASS(klass),
      G_SIGNAL_RUN_FIRST,
      0,
      NULL, NULL,
      g_cclosure_marshal_VOID__POINTER,
      G_TYPE_NONE,
      1,
      G_TYPE_POINTER);
  real_swank_session_signals[INTERACTION_UPDATED] = g_signal_new(
      "interaction-updated",
      G_TYPE_FROM_CLASS(klass),
      G_SIGNAL_RUN_FIRST,
      0,
      NULL, NULL,
      g_cclosure_marshal_VOID__POINTER,
      G_TYPE_NONE,
      1,
      G_TYPE_POINTER);
  GObjectClass *obj = G_OBJECT_CLASS(klass);
  obj->finalize = real_swank_session_finalize;
}

static void
real_swank_session_init(RealSwankSession *self)
{
  g_debug("RealSwankSession.init");
  self->proc = NULL;
  self->started = FALSE;
  self->next_tag = 1;
  self->interactions = g_hash_table_new(g_direct_hash, g_direct_equal);
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

static gchar *
unescape_string(const char *token)
{
  g_debug("RealSwankSession.unescape_string input:%s", token);
  if (*token == '"') {
    GString *out = g_string_new(NULL);
    const char *p = token + 1;
    gboolean esc = FALSE;
    for (; *p && *p != '"'; p++) {
      char c = *p;
      if (esc) {
        switch (c) {
          case 'n': g_string_append_c(out, '\n'); break;
          case 't': g_string_append_c(out, '\t'); break;
          case 'r': g_string_append_c(out, '\r'); break;
          case '\\': g_string_append_c(out, '\\'); break;
          case '"': g_string_append_c(out, '"'); break;
          default: g_string_append_c(out, c); break;
        }
        esc = FALSE;
      } else if (c == '\\') {
        esc = TRUE;
      } else {
        g_string_append_c(out, c);
      }
    }
    gchar *ret = g_string_free(out, FALSE);
    g_debug("RealSwankSession.unescape_string output:%s", ret);
    return ret;
  }
  return g_strdup(token);
}

static void
real_swank_session_eval(SwankSession *session, Interaction *interaction)
{
  g_debug("RealSwankSession.eval %s", interaction->expression);
  RealSwankSession *self = GLIDE_REAL_SWANK_SESSION(session);
  if (!self->proc)
    return;
  if (!self->started) {
    swank_process_start(self->proc);
    self->started = TRUE;
  }
  interaction->tag = self->next_tag++;
  interaction->status = INTERACTION_RUNNING;
  g_hash_table_insert(self->interactions, GUINT_TO_POINTER(interaction->tag), interaction);
  g_signal_emit(self, real_swank_session_signals[INTERACTION_ADDED], 0, interaction);
  gchar *escaped = escape_string(interaction->expression);
  gchar *rpc = g_strdup_printf("(:emacs-rex (swank:eval-and-grab-output \"%s\") \"COMMON-LISP-USER\" t %u)", escaped, interaction->tag);
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

static gboolean
parse_return_ok(const gchar *token, gchar **output, gchar **result)
{
  if (!g_str_has_prefix(token, "(:ok ")) {
    g_debug("RealSwankSession.parse_return_ok unexpected token:%s", token);
    return FALSE;
  }

  const char *p = token + strlen("(:ok ");
  gchar *list = next_token(&p);
  if (!list) {
    g_debug("RealSwankSession.parse_return_ok missing list in:%s", token);
    return FALSE;
  }

  const char *q = list;
  if (*q == '(')
    q++;
  gchar *out_tok = next_token(&q);
  gchar *res_tok = next_token(&q);
  if (*q == ')')
    q++;

  if (!out_tok || !res_tok) {
    g_debug("RealSwankSession.parse_return_ok missing tokens in:%s", list);
    g_free(list);
    g_free(out_tok);
    g_free(res_tok);
    return FALSE;
  }

  *output = unescape_string(out_tok);
  *result = unescape_string(res_tok);

  g_free(out_tok);
  g_free(res_tok);
  g_free(list);
  return TRUE;
}

static gboolean
parse_return_abort(const gchar *token, gchar **reason)
{
  if (!g_str_has_prefix(token, "(:abort ")) {
    g_debug("RealSwankSession.parse_return_abort unexpected token:%s", token);
    return FALSE;
  }

  const char *p = token + strlen("(:abort ");
  gchar *reason_tok = next_token(&p);
  if (!reason_tok) {
    g_debug("RealSwankSession.parse_return_abort missing reason in:%s", token);
    return FALSE;
  }

  *reason = unescape_string(reason_tok);
  g_free(reason_tok);
  return TRUE;
}

static void
on_return_ok(RealSwankSession *self, const gchar *output, const gchar *result,
    guint32 tag)
{
  g_debug("RealSwankSession.on_return_ok %s %s %u", output, result, tag);
  if (!self)
    return;
  Interaction *interaction =
      g_hash_table_lookup(self->interactions, GUINT_TO_POINTER(tag));
  if (!interaction) {
    g_debug("RealSwankSession.on_return_ok unknown tag:%u", tag);
    return;
  }
  g_free(interaction->output);
  g_free(interaction->result);
  interaction->output = g_strdup(output);
  interaction->result = g_strdup(result);
  interaction->status = INTERACTION_OK;
  g_signal_emit(self, real_swank_session_signals[INTERACTION_UPDATED], 0,
      interaction);
}

static void
on_return_abort(RealSwankSession *self, const gchar *reason, guint32 tag)
{
  g_debug("RealSwankSession.on_return_abort %s %u", reason, tag);
  if (!self)
    return;
  Interaction *inter = g_hash_table_lookup(self->interactions,
      GUINT_TO_POINTER(tag));
  if (inter) {
    g_free(inter->error);
    inter->error = g_strdup(reason);
    inter->status = INTERACTION_ERROR;
    g_signal_emit(self, real_swank_session_signals[INTERACTION_UPDATED], 0,
        inter);
  }
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

typedef struct {
  RealSwankSession *self;
  GString *msg;
} MessageData;

static gboolean
real_swank_session_handle_message(gpointer data)
{
  MessageData *m = data;
  g_debug_40("RealSwankSession.on_message ", m->msg->str);

  RealSwankSession *self = m->self;
  GString *msg = m->msg;

  const char *p = msg->str;
  if (g_str_has_prefix(p, "(:return ")) {
    p += strlen("(:return ");
    gchar *arg1 = next_token(&p);
    gchar *arg2 = next_token(&p);
    gchar *output = NULL;
    gchar *result = NULL;
    gchar *reason = NULL;
    gboolean handled = FALSE;
    if (parse_return_ok(arg1, &output, &result)) {
      gchar *end = NULL;
      guint64 tag64 = g_ascii_strtoull(arg2, &end, 10);
      if (end == arg2 || *end != '\0' || tag64 > G_MAXUINT32) {
        g_debug("RealSwankSession.on_message invalid tag:%s", arg2);
      } else {
        on_return_ok(self, output, result, (guint32)tag64);
        handled = TRUE;
      }
      g_free(output);
      g_free(result);
    } else if (parse_return_abort(arg1, &reason)) {
      gchar *end = NULL;
      guint64 tag64 = g_ascii_strtoull(arg2, &end, 10);
      if (end == arg2 || *end != '\0' || tag64 > G_MAXUINT32) {
        g_debug("RealSwankSession.on_message invalid tag:%s", arg2);
      } else {
        on_return_abort(self, reason, (guint32)tag64);
        handled = TRUE;
      }
      g_free(reason);
    }
    if (!handled) {
      g_debug("RealSwankSession.on_message failed to parse return list:%s", arg1);
    }
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

  g_string_free(msg, TRUE);
  if (self)
    g_object_unref(self);
  g_free(m);
  return G_SOURCE_REMOVE;
}

void
real_swank_session_on_message(GString *msg, gpointer user_data)
{
  MessageData *m = g_new(MessageData, 1);
  m->self = user_data ? GLIDE_REAL_SWANK_SESSION(user_data) : NULL;
  m->msg = g_string_new_len(msg->str, msg->len);
  if (m->self)
    g_object_ref(m->self);
  g_main_context_invoke(NULL, real_swank_session_handle_message, m);
}

