
## Documentation

- README.md describes the project and how to build it.
- doc/classes.plantuml contains the class diagram, which is a great way to understand the architecture.

## Coding Conventions

- Use glib idioms and patterns whenever possible
- Use 2 space indentation
- No space after function names in function calls.
- Unused parameters should be in `/*comments*/`.
- Keep full names for variables: it's `MockSwank *mock_swank`, not `MockSwank *ms`.
- fix all compilation warnings.
- If you're fixing a bug, add an entry to BUGS.md.

## OO

- All DI should happen in main.c, there should be no call to an `xxx_new()` in any other file.
- All dependencies should be based on interfaces, to allow for easy mocking.
- Use GObject when needed to interface with an API that requires it, but prefer light OO patterns otherwise.

## Testing

- Tests should be unit tests, in tests/, using mock objects for all dependencies.

## Common Agent mistakes

- Don't forget to add any new file to the INLINE version in main.c.
- Don't forget to run "make app-full" in src/ as part of your testing.
- Don't forget to run "make run" in tests/ as part of your testing.

