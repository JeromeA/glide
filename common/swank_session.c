#include "swank_session.h"

G_DEFINE_INTERFACE(SwankSession, swank_session, G_TYPE_OBJECT)

static void
swank_session_default_init(SwankSessionInterface * /*iface*/)
{
  g_debug("SwankSession.default_init");
}
