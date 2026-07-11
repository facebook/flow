You are building a status module split across two files: `Status.js` and `main.js`.

In `Status.js`:
- Define an enum `Status` with members `Active`, `Paused`, `Off`, and make it the default export.

In `main.js`:
- Import the `Status` enum from `./Status` and use it as both a value and a type.
- Re-export the `Status` type (only the type) so other modules can annotate with it without importing the value.
- `isActive(status: Status): boolean` — return whether the given status is `Active`.
- `parseStatus(input: string): Status | void` — convert a raw string to the enum if it is a valid member, otherwise `undefined`.
