#include "swank_process.h"

G_DEFINE_INTERFACE(SwankProcess, swank_process, G_TYPE_OBJECT)

static void
swank_process_default_init(SwankProcessInterface * /*iface*/)
{
  g_debug("SwankProcess.default_init");
}
