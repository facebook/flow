// @flow

let x = {}; // add annot
x.foo = 3;

let y = {}; // add annot
if (x) {
  y.foo = "";
}

let z = {}; // add annot
z.foo = 3;
if (x) {
  z.bar = "";
}

let q = {foo : 3}; // add annot
if (x) {
  q.bar = "";
}

let s = {foo : 3}; // should not annot, error is suppressed
if (x) {
  // $FlowFixMe[prop-missing]
  s.bar = "";
}

let d = {} // should only annot foo once
if (x) {
  d.foo = 3;
} else {
  d.foo = "";
}

let e = {};  // should only annot foo once
if (x) {
  e.foo = "";
} else {
  e.foo = "";
}

let f = {};
let field : string = f.a;

const obj =
    (() => {
      const o = {
        foo: {}, // {bar? : number, baz? : string}}
      };
      if (x) {
        o.foo.bar = 3;
      }
      return o;
    })();

obj.foo.baz = "";

(obj : {foo : {bar : number, baz : string}});

let annot : {x : number} = {}; // don't annot
