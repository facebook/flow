// @flow

declare function havoc(): void;

function test_member() {
  declare var x: { m?: (cb: (n: number) => void) => void };

  x.m && x.m(u => { (u: string); }); // error number ~> string

  if (x.m) {
    havoc();
    x.m(u => { (u: string); });  // error: undefined not a function & mising local annotation
  }

  if (x.m) {
    x.m(u => { (u: number); }); // ok
  }

  if (x.m) {
    x.m(u => { (u: string); }); // error number ~> string
  }
}

function test_optional_member() {
  declare var x: ?{ m?: (cb: (n: number) => void) => void };

  x?.m && x?.m(u => { (u: string); }); // error number ~> string

  if (x?.m) {
    havoc();
    x?.m(u => { (u: string); });  // error: undefined not a function & mising local annotation
  }

  if (x?.m) {
    x?.m(u => { (u: number); }); // ok
  }

  if (x?.m) {
    x?.m(u => { (u: string); }); // error number ~> string
  }
}
