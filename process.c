#include "process.h"

G_DEFINE_INTERFACE(Process, process, G_TYPE_OBJECT)

static void
process_default_init(ProcessInterface * /*iface*/)
{
  g_debug("Process.default_init");
}
