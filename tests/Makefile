VPATH = ../common
INCLUDES = -I../common -I../32k
CFLAGS += -Wall $(INCLUDES) `pkg-config --cflags glib-2.0 gobject-2.0 gio-2.0`
LDLIBS += `pkg-config --libs glib-2.0 gobject-2.0 gio-2.0`

TESTS = test_preferences test_process test_swank_process test_swank_session

CLEANABLES = $(TESTS)

all: $(TESTS)

test_preferences: test_preferences.c ../common/preferences.c
	$(CC) $(CFLAGS) $^ -o $@ $(LDLIBS)

test_process: test_process.c ../32k/process.c
	$(CC) $(CFLAGS) $^ -o $@ $(LDLIBS)

test_swank_process: test_swank_process.c ../32k/swank_process.c ../32k/process.c ../common/preferences.c
	$(CC) $(CFLAGS) $^ -o $@ $(LDLIBS)

test_swank_session: test_swank_session.c ../32k/swank_session.c ../32k/swank_process.c ../32k/process.c ../common/preferences.c
	$(CC) $(CFLAGS) $^ -o $@ $(LDLIBS)


clean:
	rm -f $(CLEANABLES)

.PHONY: all clean
