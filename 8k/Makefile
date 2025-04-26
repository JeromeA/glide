
LIBS += gtksourceview-4
SOURCES += file_open.c file_save.c preferences.c preferences_dialog.c find_executables.c
TARGETS += test_preferences

# file_open.c and file_save.c are reused from ../4k
VPATH += ../4k

TEST_PREFERENCES_OBJS = test_preferences.o preferences.o
CLEANABLES += $(TEST_PREFERENCES_OBJS)

include ../common/common.mk

test_preferences: $(TEST_PREFERENCES_OBJS)

