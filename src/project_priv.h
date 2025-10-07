#pragma once

#include "project.h"
#include "project_index.h"
#include "repl_session.h"
#include "asdf.h"

struct _Project {
  GPtrArray *documents; /* Document* */
  ProjectIndex *index;
  GPtrArray *event_handlers; /* ProjectEventHandler* */
  guint next_event_handler_id;
  Asdf *asdf; /* owned, nullable */
  ReplSession *repl;
  gchar *path;
  gint refcnt;
};

