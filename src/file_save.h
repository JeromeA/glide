#pragma once

typedef struct _Project Project;
typedef struct _ProjectFile ProjectFile;

void file_save(ProjectFile *file);
void file_save_all(Project *project);

