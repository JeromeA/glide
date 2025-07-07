#include "swank_session.h"
#include "swank_process.h" // For global Swank process functions
#include "interaction.h"
#include "interactions_view.h" // For calling update functions

#include <string.h>       // For strstr, etc.
#include <gtk/gtk.h>      // For g_main_context_invoke, if message handling needs to be on main thread

// Global static variables for SwankSession's state
extern GtkWidget* interactions_view_global; // Defined in main.c
static gboolean   g_swank_session_started = FALSE;
static guint32    g_swank_session_next_tag = 1;
static GHashTable *g_swank_session_interactions_table = NULL; // Stores Interaction* keyed by tag


// --- Static utility functions (copied from common/util.c) ---
static gchar *static_escape_string(const char *str) {
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

static gboolean static_ascii_isspace(char c) {
  switch ((unsigned char)c) {
    case ' ': case '\t': case '\n': case '\r': case '\f': case '\v':
      return TRUE;
    default:
      return FALSE;
  }
}

static gchar *static_next_token(const char **p_inout) {
  const char *s = *p_inout;
  while (static_ascii_isspace(*s)) s++;
  const char *start = s;

  if (*s == '\0') return NULL;

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
        if (c == '(') depth++;
        else if (c == ')') depth--;
      }
    }
  } else if (*s == '"') {
    gboolean esc = FALSE;
    s++;
    for (; *s; s++) {
      char c = *s;
      if (esc) {
        esc = FALSE;
      } else if (c == '\\') {
        esc = TRUE;
      } else if (c == '"') {
        s++;
        break;
      }
    }
  } else {
    for (; *s && !static_ascii_isspace(*s) && *s != '(' && *s != ')'; s++) {
    }
  }

  *p_inout = s;
  return g_strndup(start, s - start);
}

static gchar *static_unescape_string(const char *token) {
  if (*token == '"') {
    GString *out = g_string_new(NULL);
    const char *p = token + 1;
    gboolean esc = FALSE;
    for (; *p && (*p != '"' || esc) ; p++) {
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
    return g_string_free(out, FALSE);
  }
  return g_strdup(token);
}

// Use static versions
#define escape_string static_escape_string
#define next_token static_next_token
#define unescape_string static_unescape_string

// --- Forward declarations for internal static functions (session specific) ---
static void interaction_free_members_static(Interaction *interaction);
// swank_session_on_message_internal is now non-static and declared in swank_session.h
static gboolean swank_session_handle_message_on_main_thread(gpointer data);
static void parse_and_handle_return_message(const gchar *message_payload);
static gboolean parse_return_ok(const gchar *token, gchar **output, gchar **result);
static gboolean parse_return_abort(const gchar *token, gchar **reason);

typedef struct {
    GString *msg_payload;
} MessageDataForMainThread;

void interaction_free_members_static(Interaction *interaction) {
    if (interaction) {
        g_free(interaction->expression); interaction->expression = NULL;
        g_free(interaction->output); interaction->output = NULL;
        g_free(interaction->result); interaction->result = NULL;
        g_free(interaction->error); interaction->error = NULL;
        g_free(interaction);
    }
}

void swank_session_init_globals() {
    if (g_swank_session_interactions_table) {
        g_warning("swank_session_init_globals: Already initialized. Cleaning up old state.");
        swank_session_cleanup_globals();
    }
    g_swank_session_started = FALSE;
    g_swank_session_next_tag = 1;
    g_swank_session_interactions_table = g_hash_table_new_full(g_direct_hash,
                                                               g_direct_equal,
                                                               NULL,
                                                               (GDestroyNotify)interaction_free_members_static);
}

void swank_session_global_eval(Interaction *interaction) {
    if (!interaction || !interaction->expression) {
        g_warning("swank_session_global_eval: NULL interaction or expression.");
        if(interaction) g_free(interaction);
        return;
    }
    if (!g_swank_session_started) {
        swank_process_global_start();
        g_swank_session_started = TRUE;
    }
    interaction->tag = g_swank_session_next_tag++;
    interaction->status = INTERACTION_RUNNING;
    g_hash_table_insert(g_swank_session_interactions_table, GUINT_TO_POINTER(interaction->tag), interaction);
    if (interactions_view_global) {
        interactions_view_add_interaction(GLIDE_INTERACTIONS_VIEW(interactions_view_global), interaction);
    } else {
        g_warning("swank_session_global_eval: interactions_view_global is NULL. Cannot add interaction to view.");
    }
    gchar *escaped_expr = escape_string(interaction->expression); // Uses static_escape_string
    gchar *rpc_call = g_strdup_printf("(:emacs-rex (swank:eval-and-grab-output \"%s\") \"COMMON-LISP-USER\" t %u)",
                                      escaped_expr, interaction->tag);
    g_free(escaped_expr);
    GString *payload = g_string_new(rpc_call);
    g_free(rpc_call);
    swank_process_global_send(payload);
    g_string_free(payload, TRUE);
}

// Made non-static to be called directly from swank_process.c
void swank_session_on_message_internal(GString *msg, gpointer /*user_data*/) {
    MessageDataForMainThread *main_thread_data = g_new(MessageDataForMainThread, 1);
    main_thread_data->msg_payload = g_string_new_len(msg->str, msg->len);
    g_main_context_invoke(NULL, swank_session_handle_message_on_main_thread, main_thread_data);
}

static gboolean swank_session_handle_message_on_main_thread(gpointer data) {
    MessageDataForMainThread *main_thread_data = (MessageDataForMainThread *)data;
    GString *msg_payload = main_thread_data->msg_payload;
    const char *p = msg_payload->str;
    if (g_str_has_prefix(p, "(:return ")) {
        parse_and_handle_return_message(p);
    } else if (g_str_has_prefix(p, "(:new-features ")) {
    } else if (g_str_has_prefix(p, "(:indentation-update ")) {
    } else {
    }
    g_string_free(msg_payload, TRUE);
    g_free(main_thread_data);
    return G_SOURCE_REMOVE;
}

static void parse_and_handle_return_message(const char *message_payload_str) {
    const char *p = message_payload_str + strlen("(:return ");
    gchar *return_type_token = next_token(&p); // Uses static_next_token
    gchar *tag_id_token = next_token(&p);      // Uses static_next_token
    if (!return_type_token || !tag_id_token) {
        g_warning("swank_session: Could not parse :return message. Payload: %s", message_payload_str);
        g_free(return_type_token);
        g_free(tag_id_token);
        return;
    }
    gchar *end_ptr = NULL;
    guint64 tag_val_64 = g_ascii_strtoull(tag_id_token, &end_ptr, 10);
    guint32 tag_id = 0;
    if (*end_ptr != '\0' || tag_val_64 == 0 || tag_val_64 > G_MAXUINT32) {
        g_warning("swank_session: Invalid tag_id in :return message: '%s'", tag_id_token);
        g_free(return_type_token);
        g_free(tag_id_token);
        return;
    }
    tag_id = (guint32)tag_val_64;
    Interaction *interaction = (Interaction *)g_hash_table_lookup(g_swank_session_interactions_table, GUINT_TO_POINTER(tag_id));
    if (!interaction) {
        g_warning("swank_session: Received :return for unknown tag_id: %u", tag_id);
        g_free(return_type_token);
        g_free(tag_id_token);
        return;
    }
    gchar *output_str = NULL;
    gchar *result_str = NULL;
    gchar *abort_reason_str = NULL;
    if (parse_return_ok(return_type_token, &output_str, &result_str)) {
        g_free(interaction->output); interaction->output = output_str;
        g_free(interaction->result); interaction->result = result_str;
        interaction->status = INTERACTION_OK;
    } else if (parse_return_abort(return_type_token, &abort_reason_str)) {
        g_free(interaction->error); interaction->error = abort_reason_str;
        interaction->status = INTERACTION_ERROR;
    } else {
        g_warning("swank_session: Failed to parse specific return type: %s", return_type_token);
        interaction->status = INTERACTION_ERROR;
        g_free(interaction->error);
        interaction->error = g_strdup("Failed to parse return from Swank");
    }
    if (interactions_view_global) {
        interactions_view_update_interaction(GLIDE_INTERACTIONS_VIEW(interactions_view_global), interaction);
    } else {
        g_warning("parse_and_handle_return_message: interactions_view_global is NULL. Cannot update interaction in view.");
    }
    g_free(return_type_token);
    g_free(tag_id_token);
}

static gboolean parse_return_ok(const gchar *token, gchar **output, gchar **result) {
    if (!g_str_has_prefix(token, "(:ok ")) return FALSE;
    const char *p = token + strlen("(:ok ");
    gchar *list_payload = next_token(&p);
    if (!list_payload) return FALSE;
    if (list_payload[0] != '(' || list_payload[strlen(list_payload)-1] != ')') {
        g_free(list_payload); return FALSE;
    }
    const char *q = list_payload + 1;
    gchar *out_tok = next_token(&q);
    gchar *res_tok = next_token(&q);
    if (!out_tok || !res_tok) {
        g_free(list_payload); g_free(out_tok); g_free(res_tok); return FALSE;
    }
    *output = unescape_string(out_tok); // Uses static_unescape_string
    *result = unescape_string(res_tok); // Uses static_unescape_string
    g_free(list_payload); g_free(out_tok); g_free(res_tok);
    return TRUE;
}

static gboolean parse_return_abort(const gchar *token, gchar **reason) {
    if (!g_str_has_prefix(token, "(:abort ")) return FALSE;
    const char *p = token + strlen("(:abort ");
    gchar *reason_tok = next_token(&p);
    if (!reason_tok) return FALSE;
    *reason = unescape_string(reason_tok); // Uses static_unescape_string
    g_free(reason_tok);
    return TRUE;
}

void swank_session_cleanup_globals() {
    if (g_swank_session_interactions_table) {
        g_hash_table_destroy(g_swank_session_interactions_table);
        g_swank_session_interactions_table = NULL;
    }
    g_swank_session_started = FALSE;
    g_swank_session_next_tag = 1;
}
