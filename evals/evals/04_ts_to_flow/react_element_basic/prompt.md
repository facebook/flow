The file `source.ts` contains a TypeScript React module: a `Panel` component
whose regions accept arbitrary renderable content, and a helper that repeats a
single element a number of times.

Convert it to idiomatic Flow in `main.js`, preserving the runtime behavior and
distinguishing "any renderable content" from "a single element" where the
TypeScript does. The result must pass `flow check` with zero errors.
