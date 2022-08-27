// @flow

let tests = [
  // objects on RHS
  function() {
    ('foo' in {});
    ('foo' in { foo: null });
    (0 in {});
    (0 in { "0": null });
  },

  // arrays on RHS
  function() {
    ('foo' in ([]: Array<string>));
    (0 in ([]: Array<string>));
    ('length' in ([]: Array<string>));
  },

  // primitive classes on RHS
  function() {
    ('foo' in new String('bar'));
    ('foo' in new Number(123));
  },

  // primitives on RHS
  function() {
    ('foo' in 123); // error
    ('foo' in 'bar'); // error
    ('foo' in void 0); // error
    ('foo' in null); // error
  },

  // bogus stuff on LHS
  function() {
    (null in {}); // error
    (void 0 in {}); // error
    ({} in {}); // error
    (([]: Array<string>) in {}); // error
    (false in ([]: Array<string>)); // error
  },

  // in predicates
  function() {
    if ('foo' in 123) {} // error
    if (!'foo' in {}) {} // error, !'foo' is a boolean
    if (!('foo' in {})) {}
  },

  // annotations on RHS
  function(x: Object, y: mixed) {
    ('foo' in x); // ok
    ('foo' in y); // error
  },
]
