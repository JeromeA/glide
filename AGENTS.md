## Project Structure

- /01k to /32k: Each directory represents a specific subproject
- /common: All the common code that is shared between subprojects
- /tests: All the tests for the project

## Coding Conventions

- Use glib idioms and patterns whenever possible
- Use 2 space indentation
- No space after function names in function calls.
- Unused parameters should be in `/*comments*/`.
- Keep full names for variables: it's `MockSwank *mock_swank`, not `MockSwank *ms`.

## OO

- All DI should happen in main.c, there should be no call to an `xxx_new()` in any other file.
- All dependencies should be based on interfaces, to allow for easy mocking.

## Testing

- Tests should be unit tests, using mock objects for all dependencies.

