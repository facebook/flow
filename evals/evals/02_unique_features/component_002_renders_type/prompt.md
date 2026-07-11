Write Flow components for an alert system in `main.js`.

`Alert` takes `severity: 'info' | 'warning' | 'error'` and `message: string`. It renders a `<div>` containing:
- A `<strong>` with a label based on severity: "[ERROR]" for error, "[WARN]" for warning, "[INFO]" for info
- A `<p>` with the message text

`CompactAlert` takes `severity: 'info' | 'warning' | 'error'` and `message: string`. It truncates the message to 50 characters (appending "..." if longer), then delegates to `Alert` with the shortened message. Ensure `CompactAlert` can be used anywhere an `Alert` element is expected.

`AlertStack` takes children and wraps them in a `<div>`. Only `Alert` elements — or elements from components that produce `Alert` output — should be accepted as children.

Write an `App` that renders an `AlertStack` with a mix of `Alert` and `CompactAlert` children.

The code must pass `flow check` with zero errors.
