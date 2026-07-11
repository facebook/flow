The file `source.ts` contains a TypeScript React `FancyInput` that exposes an
imperative handle (focus/clear) to its parent through a forwarded ref.

Convert it to idiomatic Flow in `main.js`, preserving the runtime behavior — the
parent must still be able to call `focus()` and `clear()` on the input. The
result must pass `flow check` with zero errors.
