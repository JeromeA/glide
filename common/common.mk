VPATH += ../common
INCLUDE_DIRS := $(addprefix -I, $(VPATH))
LIBS += gtk+-3.0
CFLAGS += -Wall -Wextra `pkg-config --cflags $(LIBS)` -I. $(INCLUDE_DIRS)
LDLIBS += `pkg-config --libs $(LIBS)`

TARGETS += app-full app-reloc app-gdb app
SOURCES += main.c reloc.c
FULL_OBJECTS = $(SOURCES:.c=.full.o)
RELOC_OBJECTS = $(SOURCES:.c=.reloc.o)
CLEANABLES += $(TARGETS) $(FULL_OBJECTS) $(RELOC_OBJECTS) *.list app-source.* app.asm

app-full $(FULL_OBJECTS): CFLAGS += -g -DDEBUG
app-reloc $(RELOC_OBJECTS): CFLAGS += -g -DDEBUG -DRELOC -DSYSCALLS

all: $(TARGETS)

main.full.o main.reloc.o: main.c reloc.h symbols.inc
reloc.full.o reloc.reloc.o: reloc.c reloc.h symbols.inc

%.full.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@

%.reloc.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@

app-full: $(FULL_OBJECTS)
	$(CC) $^ -o $@ $(LDLIBS)

app-reloc: $(RELOC_OBJECTS)
	$(CC) $^ -o $@ $(LDLIBS)

app-source.s: CFLAGS += -O2 -DRELOC -DINLINE -DSYSCALLS -DINTEL_SYNTAX
app-source.s: $(SOURCES)
	$(CC) $(CFLAGS) main.c -S -masm=intel -o $@

app-source.asm: app-source.s libraries.asm
	../common/intel2nasm.pl $< > $@

app.asm: template.asm app-source.asm
	cat $^ > $@

app-gdb: app.asm
	nasm -DGDB $< -o $@ -l $@.list
	chmod +x $@

app: app.asm
	nasm -l $@.list $<
	chmod +x $@

clean:
	rm -f $(CLEANABLES)

