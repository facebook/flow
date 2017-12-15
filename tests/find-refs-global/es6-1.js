/* @flow */

export function foo() {

}

foo();

// This foo shadows the other one: it is not the same variable
function bar(foo) {
  console.log(foo);
}

export class Foo {
  foo(): void {}
  bar(): void {
    this.foo();
  }
}

new Foo().bar();
