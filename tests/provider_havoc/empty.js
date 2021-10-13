//@flow
// class Set<+T> {
//   add<U>(value: U): Set<T | U> {
//     return (42: any);
//   }
// }

function test() {
  let foo: {n: Set<string>} = { n: new Set() }

  let si = new Set();
  (function () { declare var x: ?string; si = si.add(x)})(); // error

  foo = { n: si };
}

function arr() {
  let arr = [];
  arr = [1,2,3]; // error
}


function arr_ok() {
  let arr = [];
  arr.push(1);
  arr = [1,2,3]; // no error, original array tvar has number as lower
}

function fn() {
  let fn = (x: number) => 42;
  function havoc() {
    fn = (y => 42); // error
  }
}

function fn_completely_unannotated() {
  let fn = x => 42;
  function havoc() {
    fn = (y => 42); // error, x has number as LB
  }
  fn(52);
}

function obj_this_empty() {
  let obj = {
    f() {
      let b = this.b; // unrelated error, but as a result b is tvar with no lowers
      b = 42; // error
    }
  }
}

function reversed() {
  let rts = [];

  function initRts(): void {
    rts = []; // error, redefinition has no lowers and its an invariant check
  }

  function getRts(): Array<number> {
    return rts;
  }
}

declare class Set<+T> {
  add<U>(U): Set<T | U>
}

function set_replace() {
  let si = new Set();

  si = new Set<string>(); // error

  (si: Set<number>);
}

function set_add() {
  let si = new Set();

  si = si.add(42); // error
}

function set_app() {
  let si = new Set();

  si = si.add(42); // error

  (si: Set<number>);
}
