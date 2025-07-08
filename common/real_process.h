#ifndef REAL_PROCESS_H
#define REAL_PROCESS_H

#include "process.h"

#define REAL_PROCESS_TYPE (real_process_get_type())
G_DECLARE_FINAL_TYPE(RealProcess, real_process, GLIDE, REAL_PROCESS, GObject)

Process *real_process_new(const gchar *cmd);
Process *real_process_new_from_argv(const gchar *const *argv);

#endif /* REAL_PROCESS_H */
