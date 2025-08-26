#pragma once

#include "node.h"

typedef struct _Project Project;

void analyse_ast(Project *project, Node *root);
void analyse_node(Project *project, Node *node, gchar **context);

