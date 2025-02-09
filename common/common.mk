VPATH += ../common
LIBS += gtk+-3.0
CFLAGS += -Wall -Wextra `pkg-config --cflags $(LIBS)` -I../common -I.
LDLIBS += `pkg-config --libs $(LIBS)`

TARGETS += app-debug app-reloc app
SOURCES += app.c reloc.c
DEBUG_OBJECTS = $(SOURCES:.c=.debug.o)
RELOC_OBJECTS = $(SOURCES:.c=.reloc.o)

app-debug $(DEBUG_OBJECTS): CFLAGS += -g -DDEBUG
app-reloc $(RELOC_OBJECTS): CFLAGS += -g -DDEBUG -DRELOC

all: $(TARGETS)

app.debug.o app.reloc.o: app.c reloc.h symbols.inc
reloc.debug.o app.reloc.o: reloc.c reloc.h symbols.inc

%.debug.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@

%.reloc.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@

app-debug: $(DEBUG_OBJECTS)
	$(CC) $^ -o $@ $(LDLIBS)

app-reloc: $(RELOC_OBJECTS)
	$(CC) $^ -o $@ $(LDLIBS)

app-source.s: CFLAGS += -O2 -DRELOC -DINLINE -DINTEL_SYNTAX
app-source.s: $(SOURCES)
	$(CC) $(CFLAGS) app.c -S -masm=intel -o $@

app-source.asm: app-source.s libraries.asm
	../common/intel2nasm.pl $< > $@

app.asm: template.asm app-source.asm
	cat $^ > $@

app: app.asm
	nasm -l app.list $<
	chmod +x $@

clean:
	rm -f $(TARGETS) $(DEBUG_OBJECTS) $(RELOC_OBJECTS) *.list app-source.* app.asm

