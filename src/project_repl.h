#pragma once

#include "project.h"

void project_request_package(Project *self, const gchar *name);
void project_request_describe(Project *self, const gchar *pkg_name,
    const gchar *symbol);

