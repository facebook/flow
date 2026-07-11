The file `source.ts` contains a TypeScript module with two generic classes: a
read-only container that only ever returns its value, and a consumer that only
ever accepts a value. The script at the bottom relies on two assignments: a
container of `Dog` is passed where a container of `Animal` is expected, and a
consumer of `Animal` is passed where a consumer of `Dog` is expected.

Convert it to idiomatic Flow in `main.js`, preserving the runtime behavior. Both
of those assignments must keep type-checking, and the result must pass
`flow check` with zero errors.
