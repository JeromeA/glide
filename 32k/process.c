#include "process.h"

G_DEFINE_INTERFACE(Process, process, G_TYPE_OBJECT)

static void
process_default_init(ProcessInterface *iface)
{
  (void)iface; /* unused */
}
