# glide

GTK Lisp IDE.

A minimal IDE for Common Lisp development. By minimal, I mean the binary is less than a fixed size, as a coding
challenge.

Running `make` in each director will build two binaries:
* app-debug, which is a standard binary, and contains all the debug information.
* app, which is a minimal binary. Not only it has no debug information, but also it has been stripped of all the
symbols, and it doesn't even have ELF sections (readelf can't read it properly).

## 1k

A minimal gtk3 application. The binary is less than 1KB, but just opens an empty window.

## 2k

A minimal IDE whose binary is less than 2KB.

It's just a text editor with CommonLisp syntax highlighting and line numbers. No other features, not even loading or
saving files.

## 4k

Same as 2k, plus:

* menu bar
* `open...`
* `save`

## 8k

Same as 4k, plus:
* persisted settings
* a settings dialog

## 16k

