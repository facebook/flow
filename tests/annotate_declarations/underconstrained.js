//@flow

function arr() {
  let arr = [];
  arr = [1,2,3];
}


function arr_ok() {
  let arr = [];
  arr.push(1);
  arr = [1,2,3];
}

function fn() {
  let fn = (x: number) => 42;
  function havoc() {
    fn = (y => 42);
  }
}

function fn_completely_unannotated() {
  let fn = x => 42;
  function havoc() {
    fn = (y => 42);
  }
  fn(52);
}

function undefined() {
  let obj = {
    f() {
      let b = this.b; // empty
      b = 42;
    }
  }
}

function reversed() {
  let rts = [];

  function initRts(): void {
    rts = [];
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

  si = new Set<string>();

  (si: Set<number>);
}

function set_add() {
  let si = new Set();

  si = si.add(42);
}

function set_app() {
  let si = new Set();

  si = si.add(42);

  (si: Set<number>);
}
