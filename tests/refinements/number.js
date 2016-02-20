// @flow

let tests = [
  function(x: number): 0 {
    if (x) {
      return x; // error
    } else {
      return x; // no error, inferred to be 0
    }
  },
];
