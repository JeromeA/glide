
ifeq ($(origin .RECIPEPREFIX), undefined)
$(error This Makefile does not support .RECIPEPREFIX (need GNU Make >= 4.0))
endif
.RECIPEPREFIX := :

LIBS += gtksourceview-4
SOURCES += \
  actions.c \
  analyse.c \
  analyse_defpackage.c \
  analyse_defun.c \
  app.c \
  asdf.c \
  document.c \
  editor.c \
  editor_container.c \
  editor_manager.c \
  editor_tooltip_window.c \
  evaluate.c \
  file_add.c \
  file_delete.c \
  file_new.c \
  file_open.c \
  file_rename.c \
  file_save.c \
  function.c \
  interactions_view.c \
  lisp_lexer.c \
  lisp_parser.c \
  lisp_parser_view.c \
  main.c \
  menu_bar.c \
  node.c \
  package.c \
  preferences.c \
  preferences_dialog.c \
  process.c \
  project.c \
  project_index.c \
  project_new_wizard.c \
  project_repl.c \
  project_view.c \
  repl_process.c \
  repl_session.c \
  status_bar.c \
  status_service.c

#SANITIZER = -fsanitize=address,undefined,bounds -fno-omit-frame-pointer
VERBOSITY = -DVERBOSITY=1

INCLUDE_DIRS := $(addprefix -I, $(VPATH))
LIBS += gtk+-3.0
CFLAGS += -Wall -Wextra `pkg-config --cflags $(LIBS)` -I. $(INCLUDE_DIRS) -g -DDEBUG $(SANITIZER) $(VERBOSITY)
LDLIBS += `pkg-config --libs $(LIBS)`
TARGETS = glide
OBJECTS = $(SOURCES:.c=.o)
CLEANABLES += $(TARGETS) $(OBJECTS)

all: $(TARGETS)

%.o: %.c
:$(CC) $(CFLAGS) -c $< -o $@

glide: $(OBJECTS)
:$(CC) $(SANITIZER) $^ -o $@ $(LDLIBS)

clean:
:rm -rf $(CLEANABLES)

