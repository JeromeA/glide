CFLAGS=-Wall -Wextra `pkg-config --cflags gtk+-3.0 gtksourceview-4`
LDLIBS=`pkg-config --libs gtk+-3.0 gtksourceview-4`

TARGETS=app-debug app-debug-reloc app

all: $(TARGETS)

app-debug: app.c
	$(CC) $(CFLAGS) $< -o $@ -g $(LDLIBS)

app-debug-reloc: app.c
	$(CC) $(CFLAGS) $< -o $@ -g $(LDLIBS) -DRELOC

app-source.s: app.c
	$(CC) $(CFLAGS) $< -S -masm=intel -o $@ -DRELOC -O2

app-source.asm: app-source.s
	./intel2nasm.pl $< > $@

app.asm: app-source.asm
	cat template.asm $< > $@

app: app.asm
	nasm -l app.list $<
	chmod +x $@

clean:
	rm -f $(TARGETS) *.list app-source.* app.asm

