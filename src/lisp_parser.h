#pragma once

// Ensure all dependency headers are processed before our own header.
#include <glib.h>
#include "node.h"

G_BEGIN_DECLS

typedef struct _LispParser LispParser;
typedef struct _ProjectFile ProjectFile;

LispParser *lisp_parser_new(void);
void lisp_parser_free(LispParser *parser);
void lisp_parser_parse(LispParser *parser, GArray *tokens, ProjectFile *file);
const Node *lisp_parser_get_ast(LispParser *parser);

G_END_DECLS

