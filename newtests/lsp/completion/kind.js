// @flow

// Tests different auto complete items use the correct `kind`.
// In VSCode, for example, this includes the autocomplete
// icons used.

let aNumber: number = 10;
class aClass { };
interface anInterface {};
type aUnion = 'a' | 'b';
let aFunction = () => null;

function foo() {
  const x = 15;
}
