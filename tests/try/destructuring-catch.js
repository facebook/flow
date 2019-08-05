/***
 * @flow
 */

// object destructuring
function f() {
  try {

  } catch ({message}) {
    message // ok
    const foo: string = 1; // it typechecks
  }
}

// array destructuring
function f() {
  try {

  } catch ([bar, baz]) {
    bar // ok
    baz // ok
    const foo: string = 1; // it typechecks
  }
}

// type annotation is banned
function f() {
  try {

  } catch ({message}: any) {
    message // ok
    const foo: string = 1; // it typechecks
  }
}
