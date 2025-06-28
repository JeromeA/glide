
SUBDIRS = 01k 02k 04k 08k 16k tests

.PHONY: all clean $(SUBDIRS) test

all: $(SUBDIRS)

$(SUBDIRS):
	$(MAKE) -C $@

clean:
	for dir in $(SUBDIRS); do \
	        $(MAKE) -C $$dir clean; \
	done

test: tests
	$(MAKE) -C tests run

