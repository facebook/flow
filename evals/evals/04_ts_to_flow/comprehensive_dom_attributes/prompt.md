The file `source.ts` contains a TypeScript React `SearchField` component whose
handlers are typed against DOM events (change, keyboard, and mouse) and whose
container accepts inline style.

Convert it to idiomatic Flow in `main.js`, preserving the runtime behavior and
keeping every event handler precisely typed to its element. The result must pass
`flow check` with zero errors.
