LIBS=gtk+-3.0
CFLAGS=-Wall -Wextra `pkg-config --cflags $(LIBS)` -O2
LDLIBS=`pkg-config --libs $(LIBS)`

TARGETS=app-debug app
SOURCES=app.c reloc.c
DEBUG_OBJECTS=$(SOURCES:.c=.o)

all: $(TARGETS)

$(DEBUG_OBJECTS): CFLAGS += -g
app.o: app.c reloc.h symbols.inc
reloc.o: reloc.c reloc.h symbols.inc

app-debug: $(DEBUG_OBJECTS)
	$(CC) $(CFLAGS) $^ -g -o $@ $(LDLIBS)

app-source.s: CFLAGS += -DRELOC -DINLINE
app-source.s: app.c
	$(CC) $(CFLAGS) $< -S -masm=intel -o $@

app-source.asm: app-source.s libraries.asm
	../common/intel2nasm.pl $< > $@

app.asm: template.asm app-source.asm
	cat $^ > $@

app: app.asm
	nasm -l app.list $<
	chmod +x $@

clean:
	rm -f $(TARGETS) $(DEBUG_OBJECTS) *.list app-source.* app.asm

