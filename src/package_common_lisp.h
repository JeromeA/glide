#ifndef PACKAGE_COMMON_LISP_H
#define PACKAGE_COMMON_LISP_H

#include "package.h"

typedef struct _SwankSession SwankSession;

void package_common_lisp_set_swank_session(SwankSession *swank);

Package *package_common_lisp_get_instance(void);

#endif // PACKAGE_COMMON_LISP_H
