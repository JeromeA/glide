#pragma once

#include <gtk/gtk.h>
#include "document.h"

typedef struct _DocumentSync DocumentSync;

DocumentSync *document_sync_new(Document *document, GtkTextBuffer *buffer);
void          document_sync_free(DocumentSync *self);
void          document_sync_update_document(DocumentSync *self);
void          document_sync_update_buffer(DocumentSync *self);
Document     *document_sync_get_document(DocumentSync *self);
GtkTextBuffer *document_sync_get_buffer(DocumentSync *self);
