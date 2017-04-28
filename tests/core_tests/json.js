// @flow

let tests = [
  // undefined input returns undefined, expected to fail
  function() {
    var a;
    (JSON.stringify(a): string);
  },
  // undefined input returns undefined
  function() {
    var a;
    (JSON.stringify(a): void);
  },
  // null input returns string
  function() {
    var a = null;
    (JSON.stringify(a): string);
  },
  // everything else returns a string
  function() {
    var a = {};
    (JSON.stringify(a): string);
  },
];
