#include "swank_session.h"
#include <glib.h>
#include <string.h>

typedef struct {
    SwankProcessImpl base;
    GString *last;
} MockSwank;

static void ms_send(SwankProcessImpl *proc, const GString *payload) {
    MockSwank *ms = (MockSwank*)proc;
    g_string_assign(ms->last, payload->str);
    g_string_truncate(ms->last, payload->len);
}
static GString *ms_get(SwankProcessImpl *proc) { return NULL; }
static void ms_free(SwankProcessImpl *proc) { MockSwank *ms = (MockSwank*)proc; g_string_free(ms->last, TRUE); g_free(ms); }

static const SwankProcess ms_iface = { ms_send, ms_get, ms_free };

static void test_eval(void)
{
    MockSwank *ms = g_new0(MockSwank,1);
    ms->base.iface = &ms_iface;
    ms->last = g_string_new(NULL);
    SwankSession *sess = swank_session_new((SwankProcessImpl*)ms);
    swank_session_eval(sess, "(+ 1 2)");
    g_assert_cmpstr(ms->last->str, ==, "(:emacs-rex \"(+ 1 2)\" \"COMMON-LISP-USER\" t 1)");
    g_object_unref(sess);
}

int main(int argc, char *argv[])
{
    g_test_init(&argc, &argv, NULL);
    g_test_add_func("/session/eval", test_eval);
    return g_test_run();
}
