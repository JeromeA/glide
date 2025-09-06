#include "repl_test_helpers.h"
#include "interaction.h"
#include "process.h"
#include <glib.h>

static void wait_interaction(Interaction *interaction) {
  int tries = 0;
  while (tries++ < 200) {
    g_mutex_lock(&interaction->lock);
    InteractionStatus status = interaction->status;
    g_mutex_unlock(&interaction->lock);
    if (status != INTERACTION_CREATED && status != INTERACTION_RUNNING)
      return;
    g_usleep(100000);
  }
}

Project *build_project(ReplSession **sess_out,
                       ReplProcess **rp_out,
                       StatusService **status_service_out) {
  const gchar *argv[] = {
    "sbcl", "--noinform",
    "--eval", "(require :asdf)",
    "--eval", "(let ((p (probe-file \"../src/\"))) (when p (pushnew p asdf:*central-registry*)))",
    "--eval", "(let ((p (probe-file \"src/\"))) (when p (pushnew p asdf:*central-registry*)))",
    "--eval", "(setf *load-verbose* nil *compile-verbose* nil)",
    "--eval", "(require :glide)",
    "--eval", "(glide:start-server)",
    NULL,
  };
  Process *proc = process_new_from_argv(argv);
  g_assert_nonnull(proc);
  ReplProcess *rp = repl_process_new(proc);
  process_unref(proc);
  StatusService *status_service = status_service_new();
  ReplSession *sess = repl_session_new(rp, status_service);
  Project *project = project_new(sess);

  if (sess_out)
    *sess_out = sess;
  else
    repl_session_unref(sess);
  if (rp_out)
    *rp_out = rp;
  else
    repl_process_unref(rp);
  if (status_service_out)
    *status_service_out = status_service;
  else
    status_service_free(status_service);
  return project;
}

void eval_form(ReplSession *sess, const gchar *form) {
  Interaction interaction;
  interaction_init(&interaction, form);
  repl_session_eval(sess, &interaction);
  wait_interaction(&interaction);
  interaction_clear(&interaction);
}
