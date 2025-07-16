VPATH = ../common
INCLUDES = -I../common -I../64k
CFLAGS += -Wall $(INCLUDES) `pkg-config --cflags glib-2.0 gobject-2.0 gio-2.0`
LDLIBS += `pkg-config --libs glib-2.0 gobject-2.0 gio-2.0`
TESTS = preferences_test process_test swank_process_test swank_session_test lisp_parser_test
CLEANABLES = $(TESTS)

all: $(TESTS)
	
preferences_test: preferences_test.c preferences.c
	$(CC) $(CFLAGS) $^ -o $@ $(LDLIBS)

process_test: process_test.c process.c real_process.c
	$(CC) $(CFLAGS) $^ -o $@ $(LDLIBS)

swank_process_test: swank_process_test.c swank_process.c real_swank_process.c process.c preferences.c
	$(CC) $(CFLAGS) $^ -o $@ $(LDLIBS)

swank_session_test: swank_session_test.c swank_session.c swank_process.c real_swank_session.c
	$(CC) $(CFLAGS) $^ -o $@ $(LDLIBS)

lisp_parser_test: lisp_parser_test.c ../64k/lisp_parser.c ../64k/string_text_provider.c ../64k/text_provider.c
	$(CC) $(CFLAGS) $^ -o $@ $(LDLIBS)

run: all
	./preferences_test
	./process_test
	./swank_process_test
	./swank_session_test
	./lisp_parser_test

clean:
	rm -f $(CLEANABLES)

.PHONY: all clean run
