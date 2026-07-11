Write a Flow function `describeError(err: Failure): string` that uses a `match` expression to turn an error into a human-readable message.

The provided class hierarchy is rooted at `AppError`, and `Failure` is the union of the concrete error types. The handling is:

- a `TimeoutError` reports how long it waited: `"Timed out after 5000ms"`
- any other `NetworkError` reports its HTTP status: `"Network error (HTTP 503)"`
- any other failure reports its message: `"Error: <message>"`

Because a `TimeoutError` is also a `NetworkError`, make sure each error is handled by the most specific case.
