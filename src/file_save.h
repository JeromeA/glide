#pragma once

typedef struct _Project Project;
typedef struct _Document Document;
typedef struct _EditorManager EditorManager;

void file_save(EditorManager *manager, Document *document);
void file_save_all(EditorManager *manager, Project *project);

