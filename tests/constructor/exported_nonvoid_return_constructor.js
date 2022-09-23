// @flow

export default class A {
  constructor(): {foo: number} { // Error: must return void
    return {foo: 3};
  }
}
