#pragma once

// Ensure all dependency headers are processed before our own header.
#include <glib.h>
#include "node.h"

G_BEGIN_DECLS

typedef struct _Document Document;

Node       *lisp_parser_parse(GArray *tokens, Document *document, Node *previous_ast,
                              gsize change_start, gsize change_end);

G_END_DECLS

