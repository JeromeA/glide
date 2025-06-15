## Project Structure for OpenAI Codex Navigation

- /01k to /32k: Each directory represents a specific subproject
- /common: All the common code that is shared between subprojects
- /tests: All the tests for the project

## Coding Conventions

- Use glib idioms and patterns whenever possible
- 2 space indentation
- All DI happens in main.c, there should be no call to an `xxx_new()` in any other file.
- Tests should be unit tests, using mock objects.

