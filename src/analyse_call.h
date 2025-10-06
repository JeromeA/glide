#pragma once

#include "document.h"
#include "node.h"

typedef struct _Project Project;

gboolean analyse_call_check(Project *project, Node *expr,
    DocumentError *error, Node **target);

