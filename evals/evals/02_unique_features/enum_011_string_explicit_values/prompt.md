Write Flow code for an analytics event system. Events are serialized to fixed lowercase wire strings that are different from the member names.

Define an enum `EventType` with these members and exact wire values:
- `PageView` → 'page_view'
- `Click` → 'click'
- `FormSubmit` → 'form_submit'
- `Dismiss` → 'dismiss'

Write:
- `parseEvent(wire: string): EventType | void` — convert a raw wire string to the event, or `undefined` if it is not a recognized event
- `toWire(event: EventType): string` — return the underlying wire string for an event
- `isInteractive(event: EventType): boolean` — return `true` for `Click` and `FormSubmit`, `false` otherwise
