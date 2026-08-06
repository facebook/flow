// @flow
// `void` is a reserved word, not a value identifier, so `[void]()` has no value
// form and is a parse error, matching a `.d.ts` body where `void` is the unary
// operator with no operand. In its own file because Flow reports only the first
// parse error per file, so it would otherwise be masked by parse_error.js.
type Bad = {[void](): void}; // ERROR: `void` has no value form
