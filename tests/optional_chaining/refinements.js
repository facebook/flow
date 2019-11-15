//@flow

type P = {c?: () => void, b?: {c?: {d: number}}};
declare var cc: P;
if (cc.b?.c) { // Refinements currently don't work in this case
  var xxx: number = cc.b.c.d;
}

declare var a: ?{b?: {c?: {d: number}, e: number, f: ?(() => number), g: ?(() => number)}};

if (a && a.b) {
  (a.b: {}); //ok
  (a.b.c: ?{}); //ok
  (a.b.c?.d: ?number); //ok
  a.b.c.d; // bad
  (a?.b.c?.d: ?number); // ok, one unneeded optional chain
  (a?.b.e: number); // ok, one unneeded optional chain
  (a.b?.e: number); // ok, one unneeded optional chain
}

if (a && a.b) {
  (a.b?.f(): ?number); // unneeded chain and bad
}

if (a && a.b) {
  (a.b.f?.(): ?number); // ok
}

if (a && a.b) {
  (a.b?.f?.(): ?number); // ok, one unneeded optional chain
}

if (a && a.b && a.b.g) {
  (a.b.g(): number); // ok
}

if (a && a.b && a.b.g) {
  (a.b.g?.(): number); // ok, unneeded chain
}

if (a && a.b && a.b.g) {
  (a.b?.g(): number); // ok, unneeded chain
}

if (a && a.b && a.b.g) {
  (a.b?.g?.(): number); // ok, two unneeded chains
} else {
  (a: {}); // should fail, sanity check
  (a: (null | void)); // should fail, sanity check
}

function f<T: any>(x: ?T) {
  if (x?.a === null) {
    return;
  }
  if (x) {
    (x.a: empty) // ok
  }
  (x.a: empty) // should fail
}
