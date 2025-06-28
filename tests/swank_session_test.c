#include "swank_session.h"
#include "swank_process.h"
#include <glib.h>
#include <string.h>

typedef struct {
  GObject parent_instance;
  GString *last;
  int start_count;
} MockSwankProcess;

typedef struct { GObjectClass parent_class; } MockSwankProcessClass;

static void mock_swank_process_start(SwankProcess *proc) {
  MockSwankProcess *mock_swank_process = (MockSwankProcess*)proc;
  mock_swank_process->start_count++;
}

static void mock_swank_process_send(SwankProcess *proc, const GString *payload) {
  MockSwankProcess *mock_swank_process = (MockSwankProcess*)proc;
  g_string_assign(mock_swank_process->last, payload->str);
  g_string_truncate(mock_swank_process->last, payload->len);
}
static GString *mock_swank_process_get(SwankProcess *proc) { (void)proc; return NULL; }

static void mock_swank_process_iface_init(SwankProcessInterface *iface) {
  iface->start = mock_swank_process_start;
  iface->send = mock_swank_process_send;
  iface->get_reply = mock_swank_process_get;
}

G_DEFINE_TYPE_WITH_CODE(MockSwankProcess, mock_swank_process, G_TYPE_OBJECT,
    G_IMPLEMENT_INTERFACE(SWANK_PROCESS_TYPE, mock_swank_process_iface_init))

static void mock_swank_process_class_init(MockSwankProcessClass *klass) {
  (void)klass;
}

static void mock_swank_process_init(MockSwankProcess *self) {
  self->last = g_string_new(NULL);
  self->start_count = 0;
}

static void test_eval(void)
{
  MockSwankProcess *mock_swank_process = g_object_new(mock_swank_process_get_type(), NULL);
  SwankSession *sess = swank_session_new(GLIDE_SWANK_PROCESS(g_object_ref(mock_swank_process)));
  swank_session_eval(sess, "(+ 1 2)");
  g_assert_cmpstr(mock_swank_process->last->str, ==,
      "(:emacs-rex (swank:eval-and-grab-output \"(+ 1 2)\") \"COMMON-LISP-USER\" t 1)");
  g_assert_cmpint(mock_swank_process->start_count, ==, 1);
  g_object_unref(sess);
  g_object_unref(mock_swank_process);
}

int main(int argc, char *argv[])
{
  g_test_init(&argc, &argv, NULL);
  g_test_add_func("/session/eval", test_eval);
  return g_test_run();
}
