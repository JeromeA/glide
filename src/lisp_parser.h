#pragma once

// Ensure all dependency headers are processed before our own header.
#include <glib.h>
#include "node.h"

G_BEGIN_DECLS

typedef struct _LispParser LispParser;
typedef struct _Document Document;

LispParser *lisp_parser_new(void);
void        lisp_parser_free(LispParser *parser);
Node       *lisp_parser_parse(LispParser *parser, GArray *tokens, Document *document);

G_END_DECLS

