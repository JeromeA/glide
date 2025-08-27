#include "glide_session.h"
#include "real_glide_session.h"
#include "glide_process.h"
#include "interaction.h"
#include "status_service.h"
#include <glib.h>
#include <string.h>

#include "interaction.h"
#include "status_service.h"
#include <glib.h>
#include <string.h>

typedef struct {
  GlideProcess base;
  GString *last;
  int start_count;
  GlideProcessMessageCallback cb;
  gpointer user_data;
} MockGlideProcess;

static void mock_glide_process_start(GlideProcess *proc) {
  MockGlideProcess *mock_glide_process = (MockGlideProcess*)proc;
  mock_glide_process->start_count++;
}

static void mock_glide_process_send(GlideProcess *proc, const GString *payload) {
  MockGlideProcess *mock_glide_process = (MockGlideProcess*)proc;
  g_string_assign(mock_glide_process->last, payload->str);
  g_string_truncate(mock_glide_process->last, payload->len);
}
static void mock_glide_process_set_cb(GlideProcess *proc,
    GlideProcessMessageCallback cb,
    gpointer user_data) {
  MockGlideProcess *mock_glide_process = (MockGlideProcess*)proc;
  mock_glide_process->cb = cb;
  mock_glide_process->user_data = user_data;
}
static void mock_glide_process_destroy(GlideProcess *proc) {
  MockGlideProcess *mock_glide_process = (MockGlideProcess*)proc;
  g_string_free(mock_glide_process->last, TRUE);
  g_free(mock_glide_process);
}

static const GlideProcessOps mock_glide_process_ops = {
  .start = mock_glide_process_start,
  .send = mock_glide_process_send,
  .set_message_cb = mock_glide_process_set_cb,
  .destroy = mock_glide_process_destroy,
};

static MockGlideProcess *mock_glide_process_new(void) {
  MockGlideProcess *self = g_new0(MockGlideProcess, 1);
  self->base.ops = &mock_glide_process_ops;
  self->base.refcnt = 1;
  self->last = g_string_new(NULL);
  self->start_count = 0;
  self->cb = NULL;
  self->user_data = NULL;
  return self;
}

static void on_interaction_updated(GlideSession * /*session*/, Interaction * /*interaction*/, gpointer user_data) {
  int *count = user_data;
  (*count)++;
}

static void on_interaction_done(Interaction * /*interaction*/, gpointer user_data) {
  int *count = user_data;
  (*count)++;
}

static void mock_send_result(MockGlideProcess *proc, const char *line) {
  if (proc->cb) {
    GString *msg = g_string_new(line);
    proc->cb(msg, proc->user_data);
    g_string_free(msg, TRUE);
  }
}

static void test_eval(void)
{
  MockGlideProcess *mock_proc = mock_glide_process_new();
  StatusService *status_service = status_service_new();
  GlideSession *sess = real_glide_session_new((GlideProcess*)mock_proc, status_service);
  Interaction interaction;
  interaction_init(&interaction, "(+ 1 2)");
  glide_session_eval(sess, &interaction);
  while (mock_proc->last->len == 0)
    g_usleep(1000);
  g_assert_cmpstr(mock_proc->last->str, ==,
      "(glide:glide-eval \"(let ((*debugger-hook* nil)) (+ 1 2))\\n\")\n");
  g_assert_cmpint(mock_proc->start_count, ==, 1);
  mock_send_result(mock_proc, "(result 3)\n");
  while (interaction.status == INTERACTION_RUNNING)
    g_usleep(1000);
  interaction_clear(&interaction);
  glide_session_unref(sess);
  status_service_free(status_service);
  glide_process_unref((GlideProcess*)mock_proc);
}

static void test_on_message_result(void)
{
  MockGlideProcess *mock_proc = mock_glide_process_new();
  StatusService *status_service = status_service_new();
  GlideSession *sess = real_glide_session_new((GlideProcess*)mock_proc, status_service);
  Interaction interaction;
  interaction_init(&interaction, "(+ 1 2)");
  glide_session_eval(sess, &interaction);
  while (interaction.status == INTERACTION_CREATED)
    g_usleep(1000);
  mock_send_result(mock_proc, "(result 5)\n");
  while (interaction.status == INTERACTION_RUNNING)
    g_usleep(1000);
  g_assert_cmpint(interaction.status, ==, INTERACTION_OK);
  g_assert_cmpstr(interaction.result, ==, "5");
  interaction_clear(&interaction);
  glide_session_unref(sess);
  status_service_free(status_service);
  glide_process_unref((GlideProcess*)mock_proc);
}

static void test_on_message_error(void)
{
  MockGlideProcess *mock_proc = mock_glide_process_new();
  StatusService *status_service = status_service_new();
  GlideSession *sess = real_glide_session_new((GlideProcess*)mock_proc, status_service);
  Interaction interaction;
  interaction_init(&interaction, "(+ 1 2)");
  glide_session_eval(sess, &interaction);
  while (interaction.status == INTERACTION_CREATED)
    g_usleep(1000);
  mock_send_result(mock_proc, "(error \"fail\")\n");
  while (interaction.status == INTERACTION_RUNNING)
    g_usleep(1000);
  g_assert_cmpint(interaction.status, ==, INTERACTION_ERROR);
  g_assert_cmpstr(interaction.error, ==, "fail");
  interaction_clear(&interaction);
  glide_session_unref(sess);
  status_service_free(status_service);
  glide_process_unref((GlideProcess*)mock_proc);
}

static void test_interaction_updated_signal(void)
{
  MockGlideProcess *proc = mock_glide_process_new();
  StatusService *status_service = status_service_new();
  GlideSession *sess = real_glide_session_new((GlideProcess*)proc, status_service);
  Interaction interaction;
  interaction_init(&interaction, "(+ 1 2)");
  glide_session_eval(sess, &interaction);
  int count = 0;
  glide_session_set_interaction_updated_cb(sess, on_interaction_updated, &count);
  while (interaction.status == INTERACTION_CREATED)
    g_usleep(1000);
  mock_send_result(proc, "(result 5)\n");
  while (interaction.status == INTERACTION_RUNNING)
    g_usleep(1000);
  g_assert_cmpint(count, ==, 1);
  interaction_clear(&interaction);
  glide_session_unref(sess);
  status_service_free(status_service);
  glide_process_unref((GlideProcess*)proc);
}

static void test_interaction_done_callback(void)
{
  MockGlideProcess *proc = mock_glide_process_new();
  StatusService *status_service = status_service_new();
  GlideSession *sess = real_glide_session_new((GlideProcess*)proc, status_service);
  Interaction interaction;
  interaction_init(&interaction, "(+ 1 2)");
  int count = 0;
  interaction.done_cb = on_interaction_done;
  interaction.done_cb_data = &count;
  glide_session_eval(sess, &interaction);
  while (interaction.status == INTERACTION_CREATED)
    g_usleep(1000);
  mock_send_result(proc, "(result 5)\n");
  while (interaction.status == INTERACTION_RUNNING)
    g_usleep(1000);
  g_assert_cmpint(count, ==, 1);
  interaction_clear(&interaction);
  glide_session_unref(sess);
  status_service_free(status_service);
  glide_process_unref((GlideProcess*)proc);
}


int main(int argc, char *argv[])
{
  g_test_init(&argc, &argv, NULL);
  g_test_add_func("/session/eval", test_eval);
  g_test_add_func("/session/on_message_result", test_on_message_result);
  g_test_add_func("/session/on_message_error", test_on_message_error);
  g_test_add_func("/session/interaction_updated_signal", test_interaction_updated_signal);
  g_test_add_func("/session/interaction_done_callback", test_interaction_done_callback);
  return g_test_run();
}
