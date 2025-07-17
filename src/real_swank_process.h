#ifndef REAL_SWANK_PROCESS_H
#define REAL_SWANK_PROCESS_H

#include "swank_process.h"

#define REAL_SWANK_PROCESS_TYPE (real_swank_process_get_type())
G_DECLARE_FINAL_TYPE(RealSwankProcess, real_swank_process, GLIDE, REAL_SWANK_PROCESS, GObject)

SwankProcess *real_swank_process_new(Process *proc, Preferences *prefs);
void real_swank_process_set_socket(RealSwankProcess *self, int fd);

#endif /* REAL_SWANK_PROCESS_H */
