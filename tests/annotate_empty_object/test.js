//@flow

declare var key : string;

let x = {}; // should annot
x[key] = 3;

let y = {};
y.key = 4;

let z = {}; // should annot
z["key"] = 5;

let a = {};
a[key] = 3;
a.key = 4;


declare var arr : Array<string>;
arr.reduce((acc, key) => { acc[key] = 4; return acc }, {}) // annot on the targ to reduce


function foo(x) {
    x[key] = 3;
}

foo({}); // annot

let b = { x : {} } // annot
b.x[key] = 3;

let first = {}; // annot
let second = first;
let third = true ? first : {}; // annot
let fourth = third;
fourth[key] = 3;

let already_annotated : { [string] : number, ... } = {}; // should not change this

let written_twice = {}; // should be { [string] : string | number }
written_twice[key] = 3;
written_twice[key] = "foo";

let written_thrice = {}; // should be { [string] : string | number }
written_thrice[key] = 3;
written_thrice[key] = "foo";
written_thrice[key] = "bar";

function Crash<T>(value: T) {
  declare var props : Array<string>;

  declare var lastProp : string;
  const lastObj = props.reduce((obj, prop) => {
    let o = obj[prop];
    if (!o) {
      o = obj[prop] = {}; // should not crash annotating this
    }
    return o;
  }, self);
  lastObj[lastProp] = value;
}

let z2 = {}; //should not annotate

declare var u : "A" | "B" | "C";
let o = {}; // should annot
o[u] = 3;

let arr2 = ["A", "B", "C"];
let x2 = {}; // should annot

for (let k of arr2) {
    x2[k] = 3
}

arr2.reduce((acc, key) => { acc[key] = 4; return acc }, {}) // annot on the targ to reduce

function bar(x) {
  x[u] = 3;
}

bar({}); // annot
