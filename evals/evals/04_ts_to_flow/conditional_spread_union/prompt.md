The file `source.ts` contains a TypeScript function that builds a transform
object, conditionally spreading in the optional array fields (`rotation`,
`scale`) only when they are present on the input, and copying those arrays so
the result shares no references with the input.

Convert it to idiomatic Flow in `main.js`, preserving the runtime behavior. The
result must pass `flow check` with zero errors, without using any
error-suppression comments.
