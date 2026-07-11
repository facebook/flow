The file `source.ts` contains a TypeScript module for a small metrics system: a
registry that maps each metric name to the shape of its samples, and a
fixed-layout row type. Both `AnySample` and `Cell` are derived as the union of
all the member types of another type.

Convert it to idiomatic Flow in `main.js`, preserving the runtime behavior and
keeping each derived union precise.
