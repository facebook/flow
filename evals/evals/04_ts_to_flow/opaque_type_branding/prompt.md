The file `source.ts` contains a TypeScript module that uses branded primitive
types so that a `UserId` and a `PostId` — both strings at runtime — can never be
mixed up at the type level.

Convert it to idiomatic Flow, preserving the runtime behavior and the guarantee
that the two id types stay distinct. Define the id types and their constructors
in a module named `ids.js`, and put the consumer code in `main.js` importing
from `ids`. Both files must pass `flow check` with zero errors.
