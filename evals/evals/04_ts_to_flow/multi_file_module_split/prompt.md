The file `source.ts` contains a TypeScript module with a `Money` type, helper
functions over it, and a small script that sums a wallet.

Convert it to idiomatic Flow split across two files: put the `Money` type and
its helper functions in `money.js`, and the script that consumes them in
`main.js` importing from `money`. Both files must pass `flow check` with zero
errors.
