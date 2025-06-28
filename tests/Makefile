VPATH = ../common
INCLUDES = -I../common -I../32k
CFLAGS += -Wall $(INCLUDES) `pkg-config --cflags glib-2.0 gobject-2.0 gio-2.0`
LDLIBS += `pkg-config --libs glib-2.0 gobject-2.0 gio-2.0`
TESTS = preferences_test process_test swank_process_test swank_session_test
CLEANABLES = $(TESTS)

all: $(TESTS)
	
preferences_test: preferences_test.c ../common/preferences.c
	$(CC) $(CFLAGS) $^ -o $@ $(LDLIBS)

process_test: process_test.c ../32k/process.c ../32k/real_process.c
	$(CC) $(CFLAGS) $^ -o $@ $(LDLIBS)

swank_process_test: swank_process_test.c ../32k/swank_process.c ../32k/real_swank_process.c ../32k/process.c ../common/preferences.c
	$(CC) $(CFLAGS) $^ -o $@ $(LDLIBS)

swank_session_test: swank_session_test.c ../32k/swank_session.c ../32k/swank_process.c
	$(CC) $(CFLAGS) $^ -o $@ $(LDLIBS)

clean:
	rm -f $(CLEANABLES)

.PHONY: all clean
