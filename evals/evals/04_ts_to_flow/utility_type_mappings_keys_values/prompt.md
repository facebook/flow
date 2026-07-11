The file `source.ts` contains a TypeScript module built around key and value
extraction: a config whose keys and values are read as types, a mapped type that
derives one validator per field, and a generic store that looks up values by key.

Convert it to idiomatic Flow in `main.js`, preserving the runtime behavior and
keeping each key and value precisely typed. The result must pass `flow check`
with zero errors.
