# glide

GTK Lisp IDE.

A compact IDE for Common Lisp development.

## Features

* a text editor with CommonLisp syntax highlighting and line numbers.
* open and save, and associated menu entries.
* persisted settings
* a settings dialog
* can evaluate selection or top-level form with alt-enter.
* extract documentation from inferior lisp process
* highlight errors in the source code
* highlight usage of symbols
* documentation tooltip on hover

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

