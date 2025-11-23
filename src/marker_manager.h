#pragma once

#include <glib.h>

typedef struct _Document Document;
typedef struct _MarkerManager MarkerManager;
typedef struct _Marker Marker;

MarkerManager *marker_manager_new(Document *document);
void           marker_manager_free(MarkerManager *manager);
Marker        *marker_manager_get_marker(MarkerManager *manager, gsize offset);
void           marker_manager_unref_marker(MarkerManager *manager, Marker *marker);
gsize          marker_get_offset(Marker *marker);
gboolean       marker_is_valid(Marker *marker);
void           marker_manager_handle_insert(MarkerManager *manager, gsize offset, gsize length);
void           marker_manager_handle_delete(MarkerManager *manager, gsize start, gsize end);

