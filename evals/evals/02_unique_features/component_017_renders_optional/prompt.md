Write Flow React components in `main.js` for a notification system.

`Toast` takes `message: string` and renders a `<div>` containing the message.

`NotificationArea` displays a single optional toast slot: it takes one parameter for the toast and renders it inside a `<div>`. The slot may hold a `Toast` element (or an element from a component that produces `Toast` output), or it may hold nothing at all. Callers must be able to pass `null` for the slot.

`MaybeToast` takes `message: string` and `visible: boolean`. It renders a `Toast` with the message when `visible` is true, and otherwise renders nothing. A `MaybeToast` element must be accepted by `NotificationArea`'s slot.

Write an `App` that renders `NotificationArea` three ways: once with a `Toast`, once with nothing, and once with a `MaybeToast`.

The code must pass `flow check` with zero errors.
