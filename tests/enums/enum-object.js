// @flow

enum E {
  A,
  B,
}

enum F {
  A,
  B,
}

// Comparison of enum object types
type EO = typeof E;
E as EO; // Valid
F as EO; // Error: types are incompatible

// Invalid access from enum object type
EO.A;

// Refinements
type VoidableEO = void | EO;

const x: VoidableEO = E;

if (typeof x === 'undefined') {
  x as void; // Valid
  x as EO; // Error
}

if (typeof x !== 'undefined') {
  x as void; // Error
  x as EO; // Valid
}

if (typeof x === 'object') {
  x as void; // Error
  x as EO; // Valid
}

if (typeof x !== 'object') {
  x as void; // Valid
  x as EO; // Error
}

if (x) {
  x as void; // Error
  x as EO; // Valid
}

if (!x) {
  x as void; // Valid
  x as EO; // Error
}

// Iteration
for (const x of E) {
  // Error
}
for (const x of E.members()) {
  // Valid
}

function* f1() {
  yield* E; // Error
}
function* f2() {
  yield* E.members(); // Valid
}

for (const x in E) {
  // Error
}

// Interface
E as interface {}; // Error
E as interface {A: E, B: E}; // Error

// `Object.values`
Object.values(E); // Error
