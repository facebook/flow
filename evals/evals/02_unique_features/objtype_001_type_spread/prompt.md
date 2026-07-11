Given the `BaseEvent` type in `main.js`, define a type `EnrichedEvent` and a function `enrich`.

`EnrichedEvent` must have every field of `BaseEvent`, plus a `tags` field (a read-only array of strings) and a numeric `priority` field. Define `EnrichedEvent` in terms of `BaseEvent` so it stays in sync if `BaseEvent`'s fields change — do not re-list `BaseEvent`'s fields.

Write `enrich(event, tags, priority): EnrichedEvent` that takes a `BaseEvent`, the tags, and the priority, and returns the enriched event.
