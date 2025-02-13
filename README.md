# glide

GTK Lisp IDE.

A minimal IDE for Common Lisp development. By minimal, I mean the binary is less than a fixed size, as a coding
challenge.

Running `make` in each directory will build 4 binaries:

* app, which is a minimal hand crafted binary, written in assembly, with a few bits coming from the C compiler. Not only
  it has no debug information, but also it has been stripped of all the symbols, it doesn't even have ELF sections
  (readelf can't read it properly), and it uses its own custom library loading code.

* app-gdb, which is the same hand crafted app, but contains a few extra bits (DT_HASH and DT_STRSZ sections) so that
  gdb doesn't complain anymore. To be used when the app above crashes, but the C version below doesn't.

* app-reloc, which uses a standard binary, as produced by the C compiler, but still uses custom library loading code.
  It has debug information, so it can be used to debug code relocation issues.

* app-debug, which is a standard binary, using C and standard linking, and contains all the debug information.

## 1k

A minimal gtk3 application. The binary is less than 1KB, it just opens an empty window.

## 2k

A minimal IDE whose binary is less than 2KB.

It's just a text editor with CommonLisp syntax highlighting and line numbers. No other features, not even loading or
saving files.

## 4k

Same as 2k, plus:

* menu bar
* `open...`
* `save`
* `save as...`

## 8k

Same as 4k, plus:
* persisted settings
* a settings dialog

## 16k

# Installation instructions

## Linux

```bash
sudo apt-get install libgtk-3-dev
sudo apt-get install libgtksourceview-4-dev
```

## MacOS

```bash
brew install gtk+3
brew install gtksourceview4
```

