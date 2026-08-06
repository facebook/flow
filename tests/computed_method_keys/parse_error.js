// @flow
// A computed method key with no value form is a parse error. Unlike a bare
// primitive keyword such as `string` (which is a value reference, see
// within.js), an array, object, or function type cannot become a value key.
// This case lives in its own file because Flow does not type check a file that
// has a parse error, so it would otherwise suppress the errors under test.
type Bad = {[string[]](): void}; // ERROR: an array type has no value form
