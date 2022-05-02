//@flow
// Spreading `null`/`undefined` is allowed.
const g: {||} = {...null}; // OK
const h: {||} = {...(null: null)}; // OK
const i: {||} = {...undefined}; // OK
const j: {||} = {...(undefined: void)}; // OK

// Spreading booleans is allowed, to support the conditional style patterns
// like the below example.
const e: {||} = {...false}; // OK
const f: {||} = {...(false: false)}; // OK

declare function showBlue(): boolean;
const styles: {backgroundColor?: string, color: string} = {...(showBlue() && {backgroundColor: 'blue'}), color: 'red'}; // OK

// Spreading numbers or strings is banned.
const a: {||} = {...3}; // ERROR
const b: {||} = {...(3: 3)}; // ERROR
const c: {||} = {...''}; // ERROR
const d: {||} = {...('': '')}; // ERROR
