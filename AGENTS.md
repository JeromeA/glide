
## Documentation

- README.md describes the project and how to build it.
- doc/classes.plantuml contains the class diagram, which is a great way to understand the architecture.

## Coding Conventions

- Use glib/gtk idioms and patterns whenever possible
- Use 2 space indentation
- No space after function names in function calls.
- Unused parameters should be in `/*comments*/`.
- Keep full names for variables: it's `MockSwank *mock_swank`, not `MockSwank *ms`.
- fix all compilation warnings.
- If you're fixing a bug, add an entry to BUGS.md.
- Wrap at 120 characters, and migrate existing code to that style when you touch it.
- All the code should assume to be on the UI thread. So, if a code is a workers on its own thread, it should call any
  callback through g_main_context_invoke(). If a callback is doing UI work (basically any file that includes gtk.h),
  it should start with an assert to ensure it's on the UI thread.
- There should be LOG and LOG_LONG entries for all operations that are advanced enough that they could fail (with
  their result or failure), so that reading the logs gives a good idea of what happened.
- When a check fail, a return or return NULL is never enough, it should at least be a g_debug, and almost always be a
  g_return_val_if_fail(), 

## OO

- All DI should happen in main.c, there should be no call to an `xxx_new()` in any other file.
- Use GObject when needed to interface with an API that requires it, but prefer light OO patterns otherwise.

## Testing

- Tests should be unit tests, in tests/, using mock objects for all dependencies.

## Common Agent mistakes

- Don't forget to add any new file to the INLINE version in main.c.
- Don't forget to run "make" in src/ as part of your testing.
- Don't forget to run "make run" in tests/ as part of your testing.
- Convert any code wrapped at 80 characters, use 120 instead.

