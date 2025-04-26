
#include "preferences.h"

void on_sdk_changed(Preferences *, const gchar *sdk, gpointer) {
  g_print("SDK changed to: %s\n", sdk);
}

int main() {
  Preferences *preferences = preferences_get_instance();
  g_print("SDK: %s\n", preferences_get_sdk(preferences));
  g_signal_connect(preferences, "sdk-changed", G_CALLBACK(on_sdk_changed), NULL);
  preferences_set_sdk(preferences, "bar");
  g_object_unref(preferences);
}
