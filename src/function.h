#pragma once

#include "node.h"

typedef enum {
  FUNCTION_KIND_FUNCTION,
  FUNCTION_KIND_COMPILED_FUNCTION,
  FUNCTION_KIND_MACRO,
  FUNCTION_KIND_SPECIAL_OPERATOR
} FunctionKind;

typedef struct Function Function;

Function *function_new(Node *symbol, Node *lambda_list,
    const gchar *doc_string, Node *type, FunctionKind kind,
    const gchar *name, const gchar *package, ProjectFile *file);
Function *function_ref(Function *function);
void function_unref(Function *function);

const Node *function_get_symbol(const Function *function);
const Node *function_get_lambda_list(const Function *function);
const gchar *function_get_doc_string(const Function *function);
const Node *function_get_type(const Function *function);
FunctionKind function_get_kind(const Function *function);
const gchar *function_get_name(const Function *function);
const gchar *function_get_package(const Function *function);
gchar       *function_tooltip(Function *function);

