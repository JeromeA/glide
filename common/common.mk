VPATH += ../common
INCLUDE_DIRS := $(addprefix -I, $(VPATH))
LIBS += gtk+-3.0
CFLAGS += -Wall -Wextra `pkg-config --cflags $(LIBS)` -I. $(INCLUDE_DIRS)
LDLIBS += `pkg-config --libs $(LIBS)`

TARGETS += app-debug app-reloc app-gdb app app-gas
SOURCES += app.c reloc.c
DEBUG_OBJECTS = $(SOURCES:.c=.debug.o)
RELOC_OBJECTS = $(SOURCES:.c=.reloc.o)
CLEANABLES += $(TARGETS) $(DEBUG_OBJECTS) $(RELOC_OBJECTS) *.list app-source.* app-source-gas.* app.asm app-gas.asm

app-debug $(DEBUG_OBJECTS): CFLAGS += -g -DDEBUG
app-reloc $(RELOC_OBJECTS): CFLAGS += -g -DDEBUG -DRELOC -DSYSCALLS

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

app-source.s: CFLAGS += -O2 -DRELOC -DINLINE -DSYSCALLS -DINTEL_SYNTAX
app-source.s: $(SOURCES)
	$(CC) $(CFLAGS) app.c -S -masm=intel -o $@

app-source-gas.s: CFLAGS += -O2 -DRELOC -DINLINE -DSYSCALLS
app-source-gas.s: $(SOURCES)
	$(CC) $(CFLAGS) app.c -S -o $@

app-source.asm: app-source.s
	../common/intel2nasm.pl $< > $@

app-gas.s: template-gas.asm app-source-gas.s
	cat $^ > $@

app-gas.asm: app-gas.s
	../common/reorder.pl $< > $@

app.asm: template.asm app-source.asm
	cat $^ > $@

app-gdb: app.asm
	nasm -DGDB $< -o $@ -l $@.list
	chmod +x $@

app: app.asm
	nasm -l app.list $<
	chmod +x $@


app-gas: app-gas.asm
	as -a=$@.list $< -o $@.o
	objcopy -O binary -j .text $@.o $@
	chmod +x $@

clean:
	rm -f $(CLEANABLES)

