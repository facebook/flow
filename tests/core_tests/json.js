// @flow

let tests = [
  // undefined input returns undefined
  function() {
    var a;
    (JSON.stringify(a): string);
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
