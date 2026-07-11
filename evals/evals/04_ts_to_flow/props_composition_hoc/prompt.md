The file `source.ts` contains a TypeScript module with two higher-order
components: one that wraps a component and forwards all of its props, and one
that injects a prop so callers no longer supply it.

Convert it to idiomatic Flow in `main.js`, preserving the runtime behavior and
keeping each wrapped component's prop types accurate. The result must pass
`flow check` with zero errors.
