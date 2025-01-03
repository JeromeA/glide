CFLAGS=-Wall -Wextra `pkg-config --cflags gtk+-3.0 gtksourceview-4` -O2
LDLIBS=`pkg-config --libs gtk+-3.0 gtksourceview-4`

TARGETS=app-debug app test_settings
SOURCES=app.c settings.c settings_dialog.c
OBJECTS=$(SOURCES:.c=.o)

all: $(TARGETS)

test_settings: test_settings.o settings.o

app-debug: $(OBJECTS)
	$(CC) $(CFLAGS) $^ -o $@ -g $(LDLIBS)

app-source.s: $(SOURCES)
	$(CC) $(CFLAGS) $< -S -masm=intel -DINLINE_ALL -o $@

app-source.asm: app-source.s
	./intel2nasm.pl $< > $@

app.asm: app-source.asm
	cat template.asm $< > $@

app: app.asm
	nasm -l app.list $<
	chmod +x $@

clean:
	rm -f $(TARGETS) *.o *.list app-source.* app.asm

