
LIBS += gtksourceview-4
SOURCES += file_open.c file_save.c settings.c settings_dialog.c
TARGETS += test_settings
VPATH += ../4k

TEST_SETTINGS_OBJS = test_settings.o settings.o
CLEANABLES += $(TEST_SETTINGS_OBJS)

include ../common/common.mk

test_settings: $(TEST_SETTINGS_OBJS)

