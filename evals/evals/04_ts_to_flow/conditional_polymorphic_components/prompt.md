The file `source.ts` contains a TypeScript React `Alert` component that renders
one of several layouts depending on a discriminated-union prop describing the
alert.

Convert it to idiomatic Flow in `main.js`, preserving the runtime behavior. The
branching must remain exhaustive over every alert variant, and the result must
pass `flow check` with zero errors.
