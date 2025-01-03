CFLAGS=-Wall -Wextra `pkg-config --cflags gtk+-3.0` -O2
LDLIBS=`pkg-config --libs gtk+-3.0`

TARGETS=app-debug app

all: $(TARGETS)

app-debug: app.c
	$(CC) $(CFLAGS) $< -o $@ -g $(LDLIBS)

app-source.s: app.c
	$(CC) $(CFLAGS) $< -S -masm=intel -o $@

app-source.asm: app-source.s
	./intel2nasm.pl $< > $@

app.asm: template.asm app-source.asm
	cat $^ > $@

app: app.asm
	nasm -l app.list $<
	chmod +x $@

clean:
	rm -f $(TARGETS) *.list app-source.* app.asm

