#include "swank_session.h"
#include "swank_process.h"
#include <glib.h>
#include <string.h>

typedef struct {
  GObject parent_instance;
  GString *last;
} MockSwank;

typedef struct { GObjectClass parent_class; } MockSwankClass;

static void ms_send(SwankProcess *proc, const GString *payload) {
  MockSwank *ms = (MockSwank*)proc;
  g_string_assign(ms->last, payload->str);
  g_string_truncate(ms->last, payload->len);
}
static GString *ms_get(SwankProcess *proc) { (void)proc; return NULL; }

static void mock_swank_iface_init(SwankProcessInterface *iface) {
  iface->send = ms_send;
  iface->get_reply = ms_get;
}

G_DEFINE_TYPE_WITH_CODE(MockSwank, mock_swank, G_TYPE_OBJECT,
    G_IMPLEMENT_INTERFACE(SWANK_PROCESS_TYPE, mock_swank_iface_init))

static void mock_swank_class_init(MockSwankClass *klass) {
  (void)klass;
}

static void mock_swank_init(MockSwank *self) {
  self->last = g_string_new(NULL);
}

static void test_eval(void)
{
  MockSwank *ms = g_object_new(mock_swank_get_type(), NULL);
  SwankSession *sess = swank_session_new(GLIDE_SWANK_PROCESS(g_object_ref(ms)));
  swank_session_eval(sess, "(+ 1 2)");
  g_assert_cmpstr(ms->last->str, ==,
      "(:emacs-rex (swank:eval-and-grab-output \"(+ 1 2)\") \"COMMON-LISP-USER\" t 1)");
  g_object_unref(sess);
  g_object_unref(ms);
}

int main(int argc, char *argv[])
{
  g_test_init(&argc, &argv, NULL);
  g_test_add_func("/session/eval", test_eval);
  return g_test_run();
}
