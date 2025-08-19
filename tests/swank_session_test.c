#include "swank_session.h"
#include "real_swank_session.h"
#include "swank_process.h"
#include "interaction.h"
#include <glib.h>
#include <string.h>

typedef struct {
  SwankProcess base;
  GString *last;
  int start_count;
  SwankProcessMessageCallback cb;
  gpointer user_data;
} MockSwankProcess;

static void mock_swank_process_start(SwankProcess *proc) {
  MockSwankProcess *mock_swank_process = (MockSwankProcess*)proc;
  mock_swank_process->start_count++;
}

static void mock_swank_process_send(SwankProcess *proc, const GString *payload) {
  MockSwankProcess *mock_swank_process = (MockSwankProcess*)proc;
  g_string_assign(mock_swank_process->last, payload->str);
  g_string_truncate(mock_swank_process->last, payload->len);
}
static void mock_swank_process_set_cb(SwankProcess *proc,
    SwankProcessMessageCallback cb,
    gpointer user_data) {
  MockSwankProcess *mock_swank_process = (MockSwankProcess*)proc;
  mock_swank_process->cb = cb;
  mock_swank_process->user_data = user_data;
}
static void mock_swank_process_destroy(SwankProcess *proc) {
  MockSwankProcess *mock_swank_process = (MockSwankProcess*)proc;
  g_string_free(mock_swank_process->last, TRUE);
  g_free(mock_swank_process);
}

static const SwankProcessOps mock_swank_process_ops = {
  .start = mock_swank_process_start,
  .send = mock_swank_process_send,
  .set_message_cb = mock_swank_process_set_cb,
  .destroy = mock_swank_process_destroy,
};

static MockSwankProcess *mock_swank_process_new(void) {
  MockSwankProcess *self = g_new0(MockSwankProcess, 1);
  self->base.ops = &mock_swank_process_ops;
  self->base.refcnt = 1;
  self->last = g_string_new(NULL);
  self->start_count = 0;
  self->cb = NULL;
  self->user_data = NULL;
  return self;
}

static void on_interaction_updated(SwankSession * /*session*/, Interaction * /*interaction*/, gpointer user_data) {
  int *count = user_data;
  (*count)++;
}

static void test_eval(void)
{
  MockSwankProcess *mock_swank_process = mock_swank_process_new();
  SwankSession *sess = real_swank_session_new((SwankProcess*)mock_swank_process);
  Interaction interaction;
  interaction_init(&interaction, "(+ 1 2)");
  swank_session_eval(sess, &interaction);
  interaction_clear(&interaction);
  g_assert_cmpint(interaction.tag, ==, 1);
  g_assert_cmpstr(mock_swank_process->last->str, ==,
      "(:emacs-rex (swank:eval-and-grab-output \"(+ 1 2)\") \"COMMON-LISP-USER\" t 1)");
  g_assert_cmpint(mock_swank_process->start_count, ==, 1);
  swank_session_unref(sess);
  swank_process_unref((SwankProcess*)mock_swank_process);
}

static void test_on_message_return_ok(void)
{
  GString *msg = g_string_new("(:return (:ok (\"\" \"5\")) 1)");
  g_test_expect_message(G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG,
      "RealSwankSession.on_return_ok  5 1");
  real_swank_session_on_message(msg, NULL);
  g_test_assert_expected_messages();
  g_string_free(msg, TRUE);
}

static void test_on_message_return_abort(void)
{
  MockSwankProcess *mock_swank_process = mock_swank_process_new();
  SwankSession *sess = real_swank_session_new((SwankProcess*)mock_swank_process);
  Interaction interaction;
  interaction_init(&interaction, "(+ 1 2)");
  swank_session_eval(sess, &interaction);
  GString *msg = g_string_new("(:return (:abort \"fail\") 1)");
  g_test_expect_message(G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG,
      "RealSwankSession.on_return_abort fail 1");
  real_swank_session_on_message(msg, sess);
  g_test_assert_expected_messages();
  g_assert_nonnull(interaction.error);
  g_assert_cmpstr(interaction.error, ==, "fail");
  interaction_clear(&interaction);
  g_string_free(msg, TRUE);
  swank_session_unref(sess);
  swank_process_unref((SwankProcess*)mock_swank_process);
}

static void test_interaction_updated_signal(void)
{
  MockSwankProcess *proc = mock_swank_process_new();
  SwankSession *sess = real_swank_session_new((SwankProcess*)proc);
  Interaction interaction;
  interaction_init(&interaction, "(+ 1 2)");
  swank_session_eval(sess, &interaction);
  int count = 0;
  swank_session_set_interaction_updated_cb(sess, on_interaction_updated, &count);
  GString *msg = g_string_new("(:return (:ok (\"\" \"5\")) 1)");
  real_swank_session_on_message(msg, sess);
  g_assert_cmpint(count, ==, 1);
  g_assert_cmpint(interaction.status, ==, INTERACTION_OK);
  interaction_clear(&interaction);
  g_string_free(msg, TRUE);
  swank_session_unref(sess);
  swank_process_unref((SwankProcess*)proc);
}

int main(int argc, char *argv[])
{
  g_test_init(&argc, &argv, NULL);
  g_test_add_func("/session/eval", test_eval);
  g_test_add_func("/session/on_message_return_ok", test_on_message_return_ok);
  g_test_add_func("/session/on_message_return_abort", test_on_message_return_abort);
  g_test_add_func("/session/interaction_updated_signal", test_interaction_updated_signal);
  return g_test_run();
}
