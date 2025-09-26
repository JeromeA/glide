# BUGS

This document lists past bugs, their symptoms, and how they were fixed. The goal is to detect patterns and avoid similar
mistakes in the future. A typical entry includes what the goal was, what went wrong, and how it was fixed.

## Editor tooltip always displayed empty sections

The editor tooltip window showed both the diagnostic and documentation areas even when only one had content. The
window called `gtk_widget_show_all` after updating the sections, which forced the hidden widgets to reappear. The
tooltip now simply shows the container, allowing each section's visibility flag to hide the empty area and separator.

## Crash while clearing the project at startup

The application crashed on startup because `editor_dispose` explicitly unreferenced its `GtkSourceView` child
widget. The `GtkScrolledWindow` parent already disposes of its children, so unreferencing the view again left GTK trying
to dispose an already finalized object, triggering the GLib critical `instance with invalid (NULL) class pointer`.

## Argument error tooltip duplicated documentation

Hovering an argument arity error showed the function documentation twice: once as escaped plain text in the error
tooltip and once in the coloured documentation section. The analyser appended the function tooltip, which already
contains the doc string, to the diagnostic message so the editor displayed both copies. The analyser now limits the
error message to the argument count details and leaves the documentation to the dedicated function tooltip.

## Error tooltips forced bold brown text

Diagnostics displayed in the editor used `<span foreground="darkred"><b>…</b></span>` markup around the message. GTK
rendered that markup in brown bold text, so the tooltip looked unlike the rest of the UI. The editor now returns the
escaped message directly and lets the theme style tooltips consistently.

## Crash when restoring last file at startup

The application crashed when restoring the previously opened file at startup.
`app_activate` tried to scroll the view by casting an `Editor` to
`GtkTextView`. Since `Editor` is a `GtkScrolledWindow` containing the
actual `GtkSourceView`, the invalid cast triggered a GLib critical and
terminated the program.

## Argument errors provided no diagnostics

Triggering an argument arity error only underlined the offending call. Hovering
the underline produced no tooltip and the log contained no trace of the error,
so diagnosing analysis problems required stepping through the code.
`document_add_error` and the error-application path now emit structured log
entries for each diagnostic, and the editor's tooltip handler displays the
associated message when hovering the underline.

## Project viewer no longer selected files

Clicking a filename in the project viewer stopped switching the notebook to that
file. The viewer's model uses a parent node labelled "src" for components, but
the selection handler still expected "components" and ignored selections. The
handler now checks for the correct "src" label so selecting a file in the
viewer updates the notebook.

## ASDF components ignored when using :file

ASDF systems that declared components with the keyword `:file` were ignored.
`node_get_name` returned the full symbol including its package prefix, so
comparisons against `file` failed. `node_get_name` now strips package
delimiters and always returns the symbol name uppercased. The ASDF parser was
updated to compare against upper case names, so `:file` and `file` components
are handled identically.

## Extend selection stuck on atoms

`Ctrl-w` extended the selection to the current symbol but further presses did
not expand to enclosing forms when invoked in the middle of an atom. The AST
contains intermediate nodes whose parents share the same range, so the
selection algorithm climbed only one level and returned the same bounds. The
search now skips parents with identical ranges and therefore reaches the
correct enclosing expression. Debug logging was also added to help diagnose
future issues.

## Crash when evaluating from the Run menu

Selecting "Run > Eval toplevel" triggered a GLib critical and terminated the
program. The "activate" signal for the menu item invoked `on_evaluate` with the
menu item as the first argument, but the handler expected only an `App*`. The
function now uses the standard `GtkWidget *, gpointer` signature and verifies
the `App` instance before evaluating the current form.

## Eval toplevel always picked first expression

Using the "Run > Eval toplevel" action evaluated the first form in the buffer
regardless of the cursor position. The search for the surrounding form scanned
backward for parentheses and stopped at the buffer start when invoked at the
beginning of a line. Evaluation now asks `Editor` for the enclosing
top‑level range, which is shared with the selection expansion logic, so the
correct expression is sent to swank.

## Eval toplevel evaluated entire buffer

Evaluating a top-level form sometimes forwarded the whole buffer to the REPL.
`editor_get_toplevel_range` walked to the parent node repeatedly until the
request reached the AST root, whose bounds match the entire buffer, and
returned that range. The parent search now stops when it reaches the root node,
so evaluation selects the actual surrounding form instead of the whole file.

## eval-and-capture evaluated raw strings

`eval-and-capture` expected a parsed form but the server sent it expressions as
strings, leading to evaluation failures. The function now reads the string into
an s-expression before evaluating it.

## Session waited for newline after prompt

SBCL's prompt is not terminated by a newline, but `RealReplProcess` only
checked complete lines during startup. The state machine therefore stalled on
"* " waiting for a newline that never arrived, leaving the server unstarted.
Startup handling now inspects the partial buffer for prompts and clears it once
consumed, so Glide no longer requires newline‑terminated prompts.

## Window height increased on each launch

The application grew taller by around 100 pixels every time it started. The
saved window size included the window manager decorations, but the restored
size interpreted the value as the client area, so the extra decoration height
was added on each run. The size allocation handler now records the interior
window size via `gtk_window_get_size`, so the saved height matches the restored
height.

## Interaction view could not scroll

Long evaluation sessions pushed earlier interactions off screen because
`InteractionsView` was a plain `GtkBox` without scrollbars. The widget now
embeds its rows in a `GtkScrolledWindow`, so previous interactions remain
accessible.

## Interactions view truncated without scrollbars

The interactions list was added directly to the `GtkScrolledWindow` without a
`GtkViewport`, so the scrollbars never appeared when the view was shorter than
its contents. Wrapping the box in a viewport allows the window to scroll.

## Interactions view hid new interactions without scrollbars

When evaluation produced many interactions, new rows were appended below the
visible area. The window neither grew nor displayed a scrollbar, so the only
way to see the new interactions was to drag the pane divider to enlarge the
view. The scrolled window now disables natural height propagation so the window
scrolls when content exceeds the available space.

## Interactions view still did not scroll

Even after disabling natural height propagation, the interactions list still
expanded with its contents and never triggered the scrollbars. The surrounding
viewport did not advertise that it could shrink, so the scrolled window kept
resizing to fit the growing box. Marking the viewport as vertically expandable
prevents this, letting the view scroll when there are many interactions.

## Interactions view packed without resizing

The interactions pane was inserted into the main window's `GtkPaned` with the
`resize` flag set to `FALSE`. As interactions accumulated, the pane grew to fit
its contents instead of allowing the embedded scrolled window to show
scrollbars. Packing the pane with `resize` set to `TRUE` and marking the widget
as vertically expandable keeps the pane at a fixed height and enables scrolling
when needed.

## Interactions view shrank with its viewport

Dragging the pane divider to reduce the interactions view did not reveal
scrollbars. The `GtkViewport` inside the view was marked as vertically
expandable, so it shrank along with the scrolled window and never exceeded its
allocation. Removing the expansion flag lets the viewport keep its natural
height, allowing the scrollbars to appear when the pane is too small.

## Interactions view left updates off screen

New interactions or additional output could appear below the visible area while
the scroll position stayed unchanged. The view now scrolls to keep the most
recent interaction in sight.

## Interactions view still scrolled unreliably

Scrolling to the latest interaction relied on an idle callback that sometimes
ran before GTK completed its resize, leaving the view above the newest content.
The interactions box now scrolls in response to its size allocating growing, so
the view reliably follows new output without race conditions.

## Stdout and stderr output did not update interactions

REPL sessions appended stdout and stderr text to the interaction but did not
invoke the update callback, so the UI missed incremental output. The session now
notifies listeners when stdout or stderr arrives.

## Debug logging split across lines

Messages containing newlines caused `g_debug_160` to emit multi-line log entries, making logs harder to read.
The helper now escapes newline characters so each debug message stays on a single line.

## ReplProcess deadlocked on incomplete messages

`ReplProcess` locked its mutex while accumulating output but returned early when the buffer lacked a complete
sexp. The missing unlock left the mutex held, so the next chunk blocked waiting for the lock and tests hung.
`on_proc_out` now releases the mutex before returning when output is incomplete, allowing processing to
continue.

## Interactions view accessed freed text

`InteractionsView` filled its widgets with pointers to strings stored in `Interaction`. The session thread
could update an interaction concurrently, freeing or replacing those strings while the UI still held references.
`Interaction` now guards its fields with a mutex, and both the session and view lock around any access,
copying text as needed to keep pointers valid.

## Package info name mismatch

`Project` asked the server for `glide:package-definition`, but the Lisp code exported `package-info`
under a different name. The missing function prevented fetching package metadata. Renaming the file and
function to `package-definition` synchronized the API and restored package lookup.
## Undo cleared buffer with no edits

Pressing `Ctrl-z` immediately after opening a file erased its contents. Loading the file text was recorded as
an undoable action, so the first undo reverted the buffer to empty. Wrapping the initial load in a
non-undoable action initializes the undo stack with the file's contents, so undo before any edits now leaves
the text unchanged.

## Project view missed package updates

Packages fetched from the REPL were added to `Project` but `ProjectView` never
repopulated, so the packages list stayed empty. `Project` now notifies listeners
when packages are added and `ProjectView` refreshes its store in response.

## repl_process_test hung on empty output

`ReplProcess` stripped leading newlines without verifying that the buffer
contained any characters. When the process emitted only newlines, the trimming
loop examined the first byte of an empty string, triggering undefined behavior
and leaving tests blocked. The loop now guards the check with the buffer's
length so empty output exits cleanly and the test completes.

## Package list updated from background thread

`ProjectView` refreshed its tree store directly from the package-added callback,
which may run outside the main GTK thread. GTK requires UI updates on the main
context, so repopulating from a background thread could lead to race conditions
and warnings. The callback now dispatches the refresh via `g_main_context_invoke`,
ensuring the tree store updates on the UI thread.

## Undo test asserted with pristine buffer

`editor_test` invoked `gtk_source_buffer_undo` on a new buffer even though the undo manager had no actions, causing a
GtkSourceView assertion. The test now avoids calling `gtk_source_buffer_undo` and simply verifies the undo stack is
empty, so pristine buffers no longer crash.

## Preferences saved during load

Loading preferences invoked the setter functions, which saved the configuration file for each key.
Initialization now disables auto-saving while preferences are loaded, avoiding unnecessary writes.

## project_document_changed cleared all indexes

Editing a file reindexed the whole project and wiped out symbol information from other files.
`project_document_changed` now scans existing indexes and removes only entries originating from the
modified file, keeping definitions from untouched files intact.

## Packages were unsorted in project view

Packages in the project viewer appeared in arbitrary order because the project
did not sort package names before populating the tree. The package list is now
sorted alphabetically so entries appear in order.

## Pruning removed known packages

`project_packages_prune` deleted any package missing from `package_defs`, which
also removed packages known to the project but defined elsewhere like
`COMMON-LISP`. The fix removes packages defined by the file being unindexed
before clearing its package definitions so external packages remain.

## `repl_session_test` required running from tests directory

Executing `repl_session_test` from the repository root failed because the test
added only `../src/` to ASDF's registry. Running the binary outside the `tests`
directory left ASDF unable to find `glide.asd`. The test now checks both
`../src/` and `src/`, registering whichever exists so it works from any
directory.

## Compilation comments treated as unknown messages

SBCL occasionally emits compilation comments beginning with `;` before
returning a result. `ReplSession` interpreted these lines as unknown messages
and logged warnings. The handler now skips leading comment lines so only the
subsequent s-expression is processed. A regression test ensures these comments
are ignored.

## Nested `defpackage` added package

`analyse-defpackage` registered packages even when the form was nested inside
other expressions. The analyser now only adds packages to the project when the
`defpackage` appears at the top level, leaving nested forms unregistered while
still tagging their symbols.

## Nested `defun` added function

`analyse_defun` registered functions even when the form appeared inside other
expressions. Functions are now added only when the `defun` is at the top level,
while nested definitions are still analysed but not recorded in the project.

## node_to_string returned lowercase symbols

`node_to_string` duplicated the text of leaf symbol nodes without normalizing case.
Parsing `aaa` therefore produced `aaa`, while other parts of the system expected
`AAA`. The function now uppercases leaf symbol and package nodes so its output
matches `node_get_name`.

## Package definition parsing only logged errors

`project_on_package_definition` treated invalid REPL output as debug messages,
making protocol bugs easy to miss. The handler now asserts that a result,
parsable AST and package name are always returned, turning these conditions
into detectable failures during development.

## Process output padded with NUL bytes

`sb-gray:stream-write-string` ignored its `start` and `end` arguments and wrote
the entire buffer back to Glide. SBCL supplied fixed-size strings padded with
`\0` bytes beyond the requested slice, so each message ballooned to 256 bytes
of output filled with NUL characters. The method now respects the slice before
forwarding, preventing padded NULs from ever reaching `Process` or
`ReplProcess`.

## Project restore only ran for first project

Loading a project after the initial startup left the notebook on the first
file, ignoring the saved cursor location. The restore logic lived in
`App.activate`, so only the first project opened during activation went through
it. Moving the restoration into `file_open_path` makes both startup and
subsequent loads follow the same code path, restoring the last file every
time.

## Project view selection leaked strings

Selecting rows in the project viewer leaked memory because the selection handler
retrieved the object's value before verifying the row type. `gtk_tree_model_get`
duplicated the underlying data for non-component rows, and the copies were never
freed. The handler now queries the row kind first and only fetches the object
for component rows, closing the leak.

## Project view repopulated on each change

`ProjectView` refreshed its tree store immediately whenever the project
signalled a change. Rapid successive signals therefore triggered redundant
updates. The callback now starts a 10 ms timer and performs the refresh once it
fires, coalescing intermediate signals.

## Process write could drop data

`process_write` performed a single `write` call and considered any short write
an error. Pipes may legally return fewer bytes than requested, truncating data
sent to child processes. The function now loops until the entire buffer is
written so partial writes no longer lose input.

## Project created off UI thread

`Project` was instantiated in `main.c` before the GTK main loop owned the
default context, causing `project_new` to fail its UI thread assertion.
Creation now happens in `App.startup`, ensuring the project is constructed on
the main thread.

## Deadlock when adding internal interactions

`InteractionsView` queued UI updates for every interaction. Internal
interactions are processed on worker threads and can hold their mutex while
the callback is dispatched to the main loop. The UI thread then blocked in
`dispatch_interaction_added` trying to take the same lock. The handler now
ignores non‑user interactions so only UI‑visible interactions schedule a
dispatch, preventing the deadlock.

## Synchronous project file analysis blocked startup

`project_document_changed` performed lexing and parsing on the UI thread, delaying
application startup when loading projects. The heavy work now happens on a
worker thread and applies results through the main loop.

## Use-after-free in project_on_package_definition

`project_on_package_definition` stored the package name pointer from the AST and
scheduled `add_package_cb`, which destroyed the parser and its nodes. The loop
describing exported symbols then dereferenced the freed pointer, corrupting the
package name and crashing. The handler now duplicates the package name before
queuing the callback and frees the copy after use.

## Async project file analysis raced with edits

`project_document_changed` parsed files on a worker thread while the UI could modify the buffer, producing
inconsistent ASTs and indexes. The analysis now runs synchronously on the UI thread to avoid races.

## Duplicate add_package_cb broke inline build

The INLINE build includes every source file in a single translation unit. Two static functions were both named
`add_package_cb`, so the compiler reported a redefinition error. Renaming the REPL callback to
`analyse_defpackage_cb` restored unique names and allowed the inline version to compile.

## Eval toplevel ignored form without trailing newline

Evaluating a buffer containing a single form without a terminating newline
returned "nothing to evaluate". `editor_get_toplevel_range` broke out of its
search loop before recording the full-buffer range, leaving start and end equal
to the cursor offset. The function now updates the range prior to checking for
the buffer bounds so end-of-file expressions evaluate correctly.

## Describe omitted lambda list

`project_on_describe` created `Function` objects without a lambda list when
reading the `describe` output for compiled functions. Tooltips therefore
lacked argument lists. The handler now parses the `Lambda-list:` section and
attaches the resulting AST to the `Function`.

## project_function_tooltip added trailing newline

Function tooltips always appended a newline after each section, leaving a blank
line at the bottom when no further content followed. Newlines are now inserted
only when another section is added, so tooltips no longer include trailing
empty lines.

## find_node_containing_range used token offsets

The AST range search function relied on token offsets and lived in `editor.c`,
so nodes without explicit tokens were skipped and the code was misplaced.
`node_find_containing_range` now resides in `node.c` and computes bounds using
`node_get_start_offset` and `node_get_end_offset` to handle all nodes.

## Last open file not restored

Glide failed to reopen the previously active file on startup. The application
only saved the current file when quitting, so unexpected termination left the
preferences without the last file and cursor position. The page switch handler
now updates both values whenever the user changes tabs, ensuring the last open
file and cursor are restored at launch.

## Reader macros parsed as symbols

Reader macros `'`, `` ` ``, `,` and `,@` were tokenized as part of the following symbols, so the
parser produced leaf symbol nodes instead of wrapping the next expression. The lexer now emits
dedicated tokens for these macros, and the parser attaches the following s-expression as their
child, yielding correct ASTs.

## DEFPACKAGE in backquote skipped code analysis

`DEFPACKAGE` forms appearing inside a backquoted expression were analysed as
package declarations, so comma expressions within them were ignored. The
analyser now treats `DEFPACKAGE` and `IN-PACKAGE` inside backquotes as regular
function calls, ensuring that comma subforms are interpreted as code.

## Toplevel analysis treated file as a list form

Analysing a file walked the artificial toplevel list as if it were code. The
file node's helper children represent each top-level form, but the analyser
tagged later children as variable uses because it treated them as arguments to
the first expression. The analyser now iterates over the toplevel children
directly, so each form is analysed independently and top-level symbols keep
their correct roles.

## COMMON-LISP package not refreshed after switching projects

Loading another project cleared the index but never asked the REPL to describe
`COMMON-LISP` again. Only the first project populated the core packages, so
standard symbols were missing afterwards. Package bootstrap now happens inside
`project_clear()`, ensuring both startup and later project loads request
`COMMON-LISP` and `COMMON-LISP-USER` from the REPL.
