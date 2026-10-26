declare function random(): boolean;

const num = 1;
//    ^?

let foo: number | string = 0;
if (random()) {
  foo = "";
}
foo;
//^?

function f(x: ?string) {
  if (x != null) {
    x;
//  ^?
  }
}

type Alias = {+f: number};
//    ^?

declare const obj: {p: ?number};
if (obj.p != null) {
  random();
  obj.p;
//    ^?
}

//^?
