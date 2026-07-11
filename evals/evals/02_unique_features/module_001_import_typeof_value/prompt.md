`numberFormat.js` default-exports a function *value* (no type is exported) that
formats a number for display.

`main.js` already has a registry of formatter functions, but the `Formatter`
type it relies on is missing. Define `Formatter` so it has the exact same type
as the value `numberFormat.js` exports. Derive it from the exported value itself
— do not re-declare the function signature by hand, and do not pull the
formatter's runtime value into `main.js` (the registry only needs its type).
