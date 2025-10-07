#pragma once

#include "project.h"
#include "project_index.h"
#include "repl_session.h"
#include "asdf.h"

struct _Project {
  GPtrArray *documents; /* Document* */
  ProjectIndex *index;
  DocumentLoadedCb document_loaded_cb;
  gpointer document_loaded_data;
  DocumentRemovedCb document_removed_cb;
  gpointer document_removed_data;
  ProjectChangedCb changed_cb;
  gpointer changed_data;
  Asdf *asdf; /* owned, nullable */
  ReplSession *repl;
  gchar *path;
  gint refcnt;
};

