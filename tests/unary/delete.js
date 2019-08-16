//@flow

let tests = [
  function (foo: { a?: number }) {
    delete foo.a; // ok
  },
  function (foo: { a?: number }) {
    delete foo['a']; // ok
  },
  function (foo: { a: number }) {
    delete foo.a; // error
  },
  function (foo: { a: number }) {
    delete foo['a']; // error
  },

  function (foo: { +a: number }) {
    delete foo.a; // error
  },
  function (foo: { +a: number }) {
    delete foo['a']; // error
  },

  function (foo: { -a?: number }) {
    delete foo.a; // ok
  },
  function (foo: { -a?: number }) {
    delete foo['a']; // ok
  },
  function (foo: { -a: number }) {
    delete foo.a; // error
  },
  function (foo: { -a: number }) {
    delete foo['a']; // error
  },

  function (foo: Array<number | void>) {
    delete foo[0] // ok
  },
  function (foo: number[]) {
    delete foo[0] // error
  },
  function (foo: [number]) {
    delete foo[1] // error
  },
  function (foo: $ReadOnlyArray<number>) {
    delete foo[0] // error
  },

  function (foo: { a?: number }) {
    delete foo?.a // TODO
  },
  function (foo: { +a?: number }) {
    delete foo?.a // TODO
  },
  function (foo: { -a?: number }) {
    delete foo?.a // TODO
  },

  function (foo: { [key: string]: number | void }) {
    delete foo['bar'] // ok
  },
  function (foo: { -[key: string]: number | void }) {
    delete foo['bar'] // ok
  },
  function (foo: { [key: string]: number }) {
    delete foo['bar'] // error
  },
  function (foo: { +[key: string]: number }) {
    delete foo['bar'] // error
  },
  function (foo: { -[key: string]: number }) {
    delete foo['bar'] // error
  },

  function (foo: { [key: number]: number | void }) {
    delete foo[0] // ok
  },
  function (foo: { [key: number]: number | void }) {
    delete foo[0] // ok
  },
  function (foo: { [key: number]: number }) {
    delete foo[0] // error
  },
  function (foo: {+[key: number]: number}) {
    delete foo[1] // error
  },
  function (foo: {-[key: number]: number}) {
    delete foo[1] // error
  },

  function (a: number) {
    delete a;
  },
  function () {
    delete { a: 1 } // error
  },
  function () {
    delete [1, 2, 3] // error
  },
  function() {
    class Foo {
      bar() {
        delete super.name;
      }
    }
  }
]
