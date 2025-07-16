# glide

GTK Lisp IDE.

A minimal IDE for Common Lisp development. By minimal, I mean the binary is less than 64k.

Running `make` will build 4 binaries:

* app, which is a minimal hand crafted binary, written in assembly, with a few bits coming from the C compiler. Not only
  it has no debug information, but also it has been stripped of all the symbols, it doesn't even have ELF sections
  (readelf can't read it properly), and it uses its own custom library loading code.

* app-gdb, which is the same hand crafted app, but contains a few extra bits (DT_HASH and DT_STRSZ sections) so that
  gdb doesn't complain anymore. To be used when the app above crashes, but the C version below doesn't.

* app-reloc, which uses a standard binary, as produced by the C compiler, but still uses custom library loading code.
  It has debug information, so it can be used to debug code relocation issues.

* app-full, which is a standard binary, using C and standard linking, and contains all the debug information.

## Features

* a text editor with CommonLisp syntax highlighting and line numbers.
* open and save, and associated menu entries.
* persisted settings
* a settings dialog
* can evaluate current line of code with alt-enter.

# Installation instructions

Here are the dependencies you need to install before building the project.

## Linux

```bash
sudo apt-get install build-essential pkg-config nasm \
    libgtk-3-dev libgtksourceview-4-dev
```

## MacOS

```bash
brew install nasm pkg-config
brew install gtk+3
brew install gtksourceview4
```

