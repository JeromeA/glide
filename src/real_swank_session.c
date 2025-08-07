#include "real_swank_session.h"
#include <string.h>
#include "util.h"
#include "interaction.h"
#include "lisp_lexer.h"
#include "lisp_parser.h"
#include "string_text_provider.h"

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

static void handle_return_message(RealSwankSession *self, const LispAstNode *ast)
{
  if (!ast || ast->children->len < 3)
    return;
  LispAstNode *result_node = g_array_index(ast->children, LispAstNode*, 1);
  LispAstNode *tag_node = g_array_index(ast->children, LispAstNode*, 2);
  if (!result_node || !tag_node || !tag_node->start_token)
    return;
  gchar *end = NULL;
  guint64 tag64 = g_ascii_strtoull(tag_node->start_token->text, &end, 10);
  if (end == tag_node->start_token->text || *end != '\0' || tag64 > G_MAXUINT32)
    return;
  guint32 tag = (guint32)tag64;
  if (result_node->type != LISP_AST_NODE_TYPE_LIST || result_node->children->len < 1)
    return;
  LispAstNode *status_node = g_array_index(result_node->children, LispAstNode*, 0);
  if (!status_node || status_node->type != LISP_AST_NODE_TYPE_SYMBOL)
    return;
  const gchar *status = status_node->start_token->text;
  if (g_strcmp0(status, ":ok") == 0) {
    if (result_node->children->len < 2)
      return;
    LispAstNode *values = g_array_index(result_node->children, LispAstNode*, 1);
    if (values->type != LISP_AST_NODE_TYPE_LIST || values->children->len < 2)
      return;
    LispAstNode *out_node = g_array_index(values->children, LispAstNode*, 0);
    LispAstNode *res_node = g_array_index(values->children, LispAstNode*, 1);
    gchar *output = unescape_string(out_node->start_token->text);
    gchar *result = unescape_string(res_node->start_token->text);
    on_return_ok(self, output, result, tag);
    g_free(output);
    g_free(result);
  } else if (g_strcmp0(status, ":abort") == 0) {
    if (result_node->children->len < 2)
      return;
    LispAstNode *reason_node = g_array_index(result_node->children, LispAstNode*, 1);
    gchar *reason = unescape_string(reason_node->start_token->text);
    on_return_abort(self, reason, tag);
    g_free(reason);
  }
}

static gboolean
real_swank_session_handle_message(gpointer data)
{
  MessageData *m = data;
  g_debug_40("RealSwankSession.on_message ", m->msg->str);

  RealSwankSession *self = m->self;
  GString *msg = m->msg;

  TextProvider *provider = string_text_provider_new(msg->str);
  LispLexer *lexer = lisp_lexer_new(provider);
  lisp_lexer_lex(lexer);
  GArray *tokens = lisp_lexer_get_tokens(lexer);
  LispParser *parser = lisp_parser_new();
  lisp_parser_parse(parser, tokens);
  const LispAstNode *ast = lisp_parser_get_ast(parser);

  if (ast && ast->children && ast->children->len > 0) {
    LispAstNode *expr = g_array_index(ast->children, LispAstNode*, 0);
    if (expr->type == LISP_AST_NODE_TYPE_LIST && expr->children && expr->children->len > 0) {
      LispAstNode *head = g_array_index(expr->children, LispAstNode*, 0);
      if (head->type == LISP_AST_NODE_TYPE_SYMBOL) {
        const gchar *sym = head->start_token->text;
        if (g_strcmp0(sym, ":return") == 0) {
          handle_return_message(self, expr);
        } else if (g_strcmp0(sym, ":new-features") == 0) {
          if (expr->children->len >= 2) {
            LispAstNode *arg_node = g_array_index(expr->children, LispAstNode*, 1);
            const LispToken *start = arg_node->start_token;
            const LispToken *end = arg_node->end_token ? arg_node->end_token : start;
            gchar *text = g_strndup(msg->str + start->start_offset, end->end_offset - start->start_offset);
            on_new_features(text);
            g_free(text);
          }
        } else if (g_strcmp0(sym, ":indentation-update") == 0) {
          if (expr->children->len >= 2) {
            LispAstNode *arg_node = g_array_index(expr->children, LispAstNode*, 1);
            const LispToken *start = arg_node->start_token;
            const LispToken *end = arg_node->end_token ? arg_node->end_token : start;
            gchar *text = g_strndup(msg->str + start->start_offset, end->end_offset - start->start_offset);
            on_indentation_update(text);
            g_free(text);
          }
        }
      }
    }
  }

  lisp_parser_free(parser);
  lisp_lexer_free(lexer);
  g_object_unref(provider);

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

