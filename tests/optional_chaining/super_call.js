class A {}
class B extends A {
  constructor () {
    super()?.a as empty; // error
  }
}
