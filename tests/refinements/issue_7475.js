// @flow

type A = {|
  "A-only": string
|};

type B = {|
  "B-only": string
|};

const foo: A = { "A-only": "bar" };

(function stuff(thing: A | B): void {
  if (thing["A-only"]) {
    // do stuff for A
    (thing: A);
    thing["A-only"]; // shouldn't error
    thing["B-only"]; // should error
  } else {
    // do stuff for A | B (NOTE: should be just B?)
    (thing: A | B);
    thing["A-only"]; // should error, but it doesn't
    thing["B-only"]; // should error
  }
})(foo);
