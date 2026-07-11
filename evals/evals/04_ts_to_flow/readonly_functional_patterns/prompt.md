The file `source.ts` contains a TypeScript module that models an immutable
product catalog and processes it with read-only functional pipelines (totals,
discounting into a new collection, and tag aggregation).

Convert it to idiomatic Flow in `main.js`, preserving the runtime behavior and
the read-only nature of every input and output. The result must pass
`flow check` with zero errors.
