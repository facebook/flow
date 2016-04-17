/* @flow */
const foo = () => {};
::foo();

class bar {
  constructor() {} 
  foo() {}
}

const x = new bar();
x::foo();
