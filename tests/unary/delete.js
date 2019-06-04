//@flow

let tests = [
  function (foo: { a: number }) {
    delete foo.a; // ok
  },
  function () {
    delete { a: 1 } // error
  },
  function () {
    delete [1, 2, 3] // error
  },
  function (foo: number[]) {
    delete foo[0] // ok
  },
  function (foo: { a?: number }) {
    delete foo?.a // ok
  },
]
