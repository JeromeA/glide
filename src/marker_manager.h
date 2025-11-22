#pragma once

#include <glib.h>

typedef struct _Document Document;
typedef struct _MarkerManager MarkerManager;
typedef struct _Marker Marker;

struct _Marker {
  gssize relative_offset; /* relative to parent, or absolute when root */
  gboolean valid;
  guint ref_count;
};

MarkerManager *marker_manager_new(Document *document);
void           marker_manager_free(MarkerManager *manager);
Marker        *marker_manager_get_marker(MarkerManager *manager, gsize offset);
void           marker_manager_unref_marker(MarkerManager *manager, Marker *marker);
gsize          marker_get_offset(Marker *marker);
gboolean       marker_manager_is_valid(MarkerManager *manager, Marker *marker);
void           marker_manager_handle_insert(MarkerManager *manager, gsize offset, gsize length);
void           marker_manager_handle_delete(MarkerManager *manager, gsize start, gsize end);

