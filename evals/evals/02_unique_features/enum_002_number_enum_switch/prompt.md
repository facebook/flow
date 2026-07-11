Write Flow code for an HTTP status classifier.

Define an enum `HttpStatus` of number with members: `Ok` (200), `Created` (201), `BadRequest` (400), `Unauthorized` (401), `NotFound` (404), `InternalError` (500).

Write:
- `isSuccess(status: HttpStatus): boolean` — return `true` for 2xx codes, `false` otherwise
- `toStatusLine(status: HttpStatus): string` — return the status as a string like `"200 OK"`, `"404 Not Found"`, etc. Don't duplicate the enum numeric values in the message — convert the enum value to its underlying number to build this string.
- `retryable(status: HttpStatus): boolean` — return `true` only for `InternalError` (server can recover), `false` for all others
