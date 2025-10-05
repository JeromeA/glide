# Glide

Gtk Lisp IDE.

A compact IDE for Common Lisp development.

Contrary to SLIME and other Lisp or Emacs based IDEs, Glide builds all its features on static analysis of the source
code, which allows it to know about the code without running it. This makes it faster and more responsive.

## Features

* a text editor with CommonLisp syntax highlighting.
* ASDF project files.
* can evaluate selection or top-level form with alt-enter.
* extract documentation from inferior lisp process
* highlight errors in the source code
* highlight usage of symbols
* documentation tooltip on hover

## Limitations

In addition to being alpha software, Glide will always have the following limitations due to its static analysis approach:
* DEFPACKAGE are always assumed to be COMMON-LISP:DEFPACKAGE, so that we can bootstrap the package system.
* DEFMACRO must be wihtout side effects, so that we can expand them at parse time.
* The current package is always assumed to be either explicit with IN-PACKAGE, or it's COMMON-LISP-USER.

# Installation instructions

Here are the dependencies you need to install before building the project.

## Linux

```bash
sudo apt-get install build-essential pkg-config libgtk-3-dev libgtksourceview-4-dev
```

## MacOS

```bash
brew install pkg-config gtk+3 gtksourceview4
```

