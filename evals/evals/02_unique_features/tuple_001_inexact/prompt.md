Three event tuples are defined in `main.js`. Each one begins with an event name (a string) followed by a timestamp (a number), but they carry different amounts of additional trailing data.

Write a function `describe(event)` that returns the string `"<name>@<timestamp>"`, using only the first two elements.

`describe` must accept all three event tuples even though they have different lengths. Then export an array `summaries: Array<string>` holding the result of calling `describe` on each of the three events.
