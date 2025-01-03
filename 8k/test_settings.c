
#include "settings.h"

void on_sdk_changed(Settings *, const gchar *sdk, gpointer) {
  g_print("SDK changed to: %s\n", sdk);
}

int main() {
  Settings *settings = settings_get_instance();
  g_print("SDK: %s\n", settings_get_sdk(settings));
  g_signal_connect(settings, "sdk-changed", G_CALLBACK(on_sdk_changed), NULL);
  settings_set_sdk(settings, "bar");
  g_object_unref(settings);
}
