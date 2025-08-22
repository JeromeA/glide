#include "real_swank_session.h"
#include <string.h>
#include "util.h"
#include "interaction.h"
#include "lisp_lexer.h"
#include "lisp_parser.h"
#include "string_text_provider.h"

static void real_swank_session_eval(SwankSession *session, Interaction *interaction);
static void real_swank_session_set_added(SwankSession *session, SwankSessionCallback cb, gpointer user_data);
static void real_swank_session_set_updated(SwankSession *session, SwankSessionCallback cb, gpointer user_data);
static void real_swank_session_destroy(SwankSession *session);

static const SwankSessionOps real_swank_session_ops = {
  .eval = real_swank_session_eval,
  .set_interaction_added_cb = real_swank_session_set_added,
  .set_interaction_updated_cb = real_swank_session_set_updated,
  .destroy = real_swank_session_destroy,
};

SwankSession *real_swank_session_new(SwankProcess *proc, StatusService *status_service) {
  g_debug("RealSwankSession.new");
  RealSwankSession *self = g_new0(RealSwankSession, 1);
  self->base.ops = &real_swank_session_ops;
  self->base.refcnt = 1;
  self->proc = proc ? swank_process_ref(proc) : NULL;
  self->status_service = status_service;
  self->started = FALSE;
  self->next_tag = 1;
  self->interactions = g_hash_table_new(g_direct_hash, g_direct_equal);
  self->added_cb = NULL;
  self->added_cb_data = NULL;
  self->updated_cb = NULL;
  self->updated_cb_data = NULL;
  if (self->proc)
    swank_process_set_message_cb(self->proc, real_swank_session_on_message, self);
  return (SwankSession*)self;
}

static gchar *escape_string(const char *str) {
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

static gchar *unescape_string(const char *token) {
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

static void real_swank_session_eval(SwankSession *session, Interaction *interaction) {
  g_debug("RealSwankSession.eval %s", interaction->expression);
  RealSwankSession *self = (RealSwankSession*)session;
  if (!self->proc)
    return;
  if (!self->started) {
    guint status_id = status_service_publish(self->status_service, "SBCL is starting...");
    swank_process_start(self->proc);
    status_service_unpublish(self->status_service, status_id);
    self->started = TRUE;
  }
  interaction->tag = self->next_tag++;
  interaction->status = INTERACTION_RUNNING;
  g_hash_table_insert(self->interactions, GUINT_TO_POINTER(interaction->tag), interaction);
  if (self->added_cb)
    self->added_cb(session, interaction, self->added_cb_data);
  gchar *escaped = escape_string(interaction->expression);
  gchar *rpc = g_strdup_printf("(:emacs-rex (swank:eval-and-grab-output \"%s\") \"COMMON-LISP-USER\" t %u)", escaped, interaction->tag);
  GString *payload = g_string_new(rpc);
  g_free(escaped);
  g_free(rpc);
  swank_process_send(self->proc, payload);
  g_string_free(payload, TRUE);
}

static void on_return_ok(RealSwankSession *self, const gchar *output, const gchar *result, guint32 tag) {
  g_debug("RealSwankSession.on_return_ok %s %s %u", output, result, tag);
  if (!self)
    return;
  Interaction *interaction = g_hash_table_lookup(self->interactions, GUINT_TO_POINTER(tag));
  if (!interaction) {
    g_debug("RealSwankSession.on_return_ok unknown tag:%u", tag);
    return;
  }
  g_free(interaction->output);
  g_free(interaction->result);
  interaction->output = g_strdup(output);
  interaction->result = g_strdup(result);
  interaction->status = INTERACTION_OK;
  if (self->updated_cb)
    self->updated_cb((SwankSession*)self, interaction, self->updated_cb_data);
  if (interaction->done_cb)
    interaction->done_cb(interaction, interaction->done_cb_data);
}

static void on_return_abort(RealSwankSession *self, const gchar *reason, guint32 tag) {
  g_debug("RealSwankSession.on_return_abort %s %u", reason, tag);
  if (!self)
    return;
  Interaction *interaction = g_hash_table_lookup(self->interactions, GUINT_TO_POINTER(tag));
  if (!interaction) {
    g_debug("RealSwankSession.on_return_abort unknown tag:%u", tag);
    return;
  }
  g_free(interaction->error);
  interaction->error = g_strdup(reason);
  interaction->status = INTERACTION_ERROR;
  if (self->updated_cb)
    self->updated_cb((SwankSession*)self, interaction, self->updated_cb_data);
  if (interaction->done_cb)
    interaction->done_cb(interaction, interaction->done_cb_data);
}

static void real_swank_session_set_added(SwankSession *session, SwankSessionCallback cb, gpointer user_data) {
  RealSwankSession *self = (RealSwankSession*)session;
  self->added_cb = cb;
  self->added_cb_data = user_data;
}

static void real_swank_session_set_updated(SwankSession *session, SwankSessionCallback cb, gpointer user_data) {
  RealSwankSession *self = (RealSwankSession*)session;
  self->updated_cb = cb;
  self->updated_cb_data = user_data;
}

static void real_swank_session_destroy(SwankSession *session) {
  g_debug("RealSwankSession.destroy");
  RealSwankSession *self = (RealSwankSession*)session;
  if (self->proc)
    swank_process_unref(self->proc);
  if (self->interactions)
    g_hash_table_destroy(self->interactions);
  g_free(self);
}

void real_swank_session_on_message(GString *msg, gpointer user_data) {
  RealSwankSession *self = user_data ? (RealSwankSession*)user_data : NULL;
  g_debug_40("RealSwankSession.on_message msg:", msg->str);
  const char *str = msg->str;
  if (g_str_has_prefix(str, "(:return (:ok (\"\" \"")) {
    const char *res_start = str + strlen("(:return (:ok (\"\" \"");
    const char *end = strstr(res_start, "\")) ");
    if (!end) return;
    gchar *result = g_strndup(res_start, end - res_start);
    guint32 tag = (guint32)g_ascii_strtoll(end + 4, NULL, 10);
    on_return_ok(self, "", result, tag);
    g_free(result);
  } else if (g_str_has_prefix(str, "(:return (:abort \"")) {
    const char *reason_start = str + strlen("(:return (:abort \"");
    const char *end = strstr(reason_start, "\") ");
    if (!end) return;
    gsize len = end - reason_start;
    gchar *reason = g_strndup(reason_start, len);
    guint32 tag = (guint32)g_ascii_strtoll(end + 3, NULL, 10);
    on_return_abort(self, reason, tag);
    g_free(reason);
  }
}
