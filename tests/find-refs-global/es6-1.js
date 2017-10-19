/* @flow */

export function foo() {

}

foo();

// This foo shadows the other one: it is not the same variable
function bar(foo) {
  console.log(foo);
}
