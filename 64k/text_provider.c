#include "text_provider.h"

static void text_provider_default_init(TextProviderInterface *iface) {}

G_DEFINE_INTERFACE(TextProvider, text_provider, G_TYPE_OBJECT)
