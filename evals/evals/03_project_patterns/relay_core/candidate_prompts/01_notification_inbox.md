You're building a notification inbox for an internal tool.

A server-side Relay query has already been preloaded and a reference to it will be passed into your top-level component. The query (`NotificationInboxQuery`) returns a list of notification objects.

Your job is to write two components:

**`NotificationInbox.react.js`**
- Accepts a preloaded query reference for `NotificationInboxQuery`
- Reads the query data and renders a list of `NotificationItem` components
- Passes each notification's fragment reference down to `NotificationItem`

**`NotificationItem.react.js`**
- Accepts a fragment reference for `NotificationItem_notification`
- Reads the fragment data and renders the notification's title, timestamp, and read/unread status

Both components must:
- Use `@flow strict-local`
- Use component syntax (the `component` keyword)
- Import Relay hooks from `RelayHooks`

Run `flow` to verify your code type-checks with zero errors.
