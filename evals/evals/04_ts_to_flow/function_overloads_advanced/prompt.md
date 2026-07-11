The file `source.ts` contains a TypeScript module that uses overloaded function
signatures for a container factory and a token accessor, plus a bounded-generic
lookup helper.

Convert it to idiomatic Flow in `main.js`, preserving the runtime behavior. The
result must pass `flow check` with zero errors, and each call site must keep its
precise return type (for example, reading the `"spacing"` token must still
produce a `number`). Express each accessor as a single generic function whose
return type is derived from its arguments, rather than repeating a separate
signature for every key.
