`main.js` defines an `Event` type: a disjoint union of a `click` event (with a `left`/`right` button) and a `key` event (with an `up`/`down` button).

Write a Flow function `describeEvent(event: Event): string` that uses `match` to describe an event, covering all combinations of the two sentinel properties (`kind` and `button`).
