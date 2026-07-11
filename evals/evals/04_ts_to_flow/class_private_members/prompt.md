The file `source.ts` contains a TypeScript program that models a fixed-window
rate limiter, tracking hits per client id and refusing calls once a client
exceeds a per-window budget.

Convert it to idiomatic Flow in `main.js`, preserving the runtime behavior.
