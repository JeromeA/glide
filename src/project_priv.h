#pragma once

#include "project.h"
#include "project_index.h"
#include "repl_session.h"
#include "asdf.h"

struct _Project {
  GPtrArray *files; /* ProjectFile* */
  ProjectIndex *index;
  ProjectFileLoadedCb file_loaded_cb;
  gpointer file_loaded_data;
  ProjectFileRemovedCb file_removed_cb;
  gpointer file_removed_data;
  ProjectFileChangedCb file_changed_cb;
  gpointer file_changed_data;
  ProjectChangedCb changed_cb;
  gpointer changed_data;
  Asdf *asdf; /* owned, nullable */
  ReplSession *repl;
  gchar *path;
  gint refcnt;
};

