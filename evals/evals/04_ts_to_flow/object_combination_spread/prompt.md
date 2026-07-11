The file `source.ts` contains a TypeScript program that layers a base HTTP
retry policy with per-endpoint jitter and auth settings, then computes the
effective delay for a series of retry attempts.

Convert it to idiomatic Flow in `main.js`, preserving the runtime behavior.
