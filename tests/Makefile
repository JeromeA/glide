VPATH = ../src
INCLUDES = -I$(VPATH) -I..
CFLAGS += -Wall $(INCLUDES) `pkg-config --cflags glib-2.0 gobject-2.0 gio-2.0 gtk+-3.0`
LDLIBS += `pkg-config --libs glib-2.0 gobject-2.0 gio-2.0 gtk+-3.0`
TESTS = preferences_test process_test swank_process_test swank_session_test lisp_parser_test project_test asdf_test analyser_test package_test package_common_lisp_test status_service_test
CLEANABLES = $(TESTS)

all: $(TESTS)

preferences_test: preferences_test.c preferences.c
	$(CC) $(CFLAGS) $^ -o $@ $(LDLIBS)

process_test: process_test.c process.c real_process.c
	$(CC) $(CFLAGS) $^ -o $@ $(LDLIBS)

swank_process_test: swank_process_test.c swank_process.c real_swank_process.c process.c preferences.c
	$(CC) $(CFLAGS) $^ -o $@ $(LDLIBS)

swank_session_test: swank_session_test.c swank_session.c swank_process.c real_swank_session.c status_service.c lisp_lexer.c lisp_parser.c node.c package.c string_text_provider.c text_provider.c
	$(CC) $(CFLAGS) $^ -o $@ $(LDLIBS)

lisp_parser_test: lisp_parser_test.c lisp_lexer.c lisp_parser.c node.c package.c string_text_provider.c text_provider.c
	$(CC) $(CFLAGS) $^ -o $@ $(LDLIBS)

project_test: project_test.c project.c project_file.c analyse.c analyse_defpackage.c analyse_defun.c node.c package.c lisp_lexer.c lisp_parser.c string_text_provider.c text_provider.c
	$(CC) $(CFLAGS) $^ -o $@ $(LDLIBS)

asdf_test: asdf_test.c asdf.c string_text_provider.c text_provider.c lisp_lexer.c lisp_parser.c node.c package.c project.c project_file.c analyse.c analyse_defpackage.c analyse_defun.c
	$(CC) $(CFLAGS) $^ -o $@ $(LDLIBS)

analyser_test: analyser_test.c analyse.c analyse_defpackage.c analyse_defun.c node.c package.c lisp_lexer.c lisp_parser.c string_text_provider.c text_provider.c project.c project_file.c
	$(CC) $(CFLAGS) $^ -o $@ $(LDLIBS)

package_test: package_test.c package.c package_common_lisp_user.c node.c
	$(CC) $(CFLAGS) $^ -o $@ $(LDLIBS)

package_common_lisp_test: package_common_lisp_test.c package_common_lisp.c package.c swank_session.c
	$(CC) $(CFLAGS) $^ -o $@ $(LDLIBS)

status_service_test: status_service_test.c status_service.c
	$(CC) $(CFLAGS) $^ -o $@ $(LDLIBS)

run: all
	./preferences_test
	./process_test
	./swank_process_test
	./swank_session_test
	./lisp_parser_test
	./project_test
	./asdf_test
	./analyser_test
	./package_test
	./package_common_lisp_test
	./status_service_test

clean:
	rm -f $(CLEANABLES)

.PHONY: all clean run
