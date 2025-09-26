#pragma once

typedef struct _Project Project;
typedef struct _ProjectFile ProjectFile;
typedef struct _EditorManager EditorManager;

void file_save(EditorManager *manager, ProjectFile *file);
void file_save_all(EditorManager *manager, Project *project);

