// @flow

// String (the class) tests. string (the literals) are not part of core.js

let tests = [
  // codePointAt
  function() {
    // fails
    (''.codePointAt(123): number);
  },

  // index
  function() {
    // fails
    (new String()[123]: string);
  },
];
