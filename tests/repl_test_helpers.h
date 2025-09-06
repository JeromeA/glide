#pragma once

#include "project.h"
#include "repl_session.h"
#include "repl_process.h"
#include "status_service.h"

Project *build_project(ReplSession **sess_out,
                       ReplProcess **rp_out,
                       StatusService **status_service_out);

void eval_form(ReplSession *sess, const gchar *form);
