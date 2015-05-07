/* @flow */

var Foo = {
  a: Foo.b('Foo'),    // no annotation required
  b: function(arg) {  // missing arg and return annotations
    return arg;
  },

  // object property types are inferred, so make sure that this doesn't cause
  // us to also infer the parameter's type.
  c: Foo.d('bar'),    // no annotation required
  d: function(arg) {  // missing arg and return annotations
    return {
      bar: arg
    };
  },

  // reverse the alphabetical order of the keys, in case the order that we
  // traverse Foo's properties matters.
  f: Foo.e('bar'),    // no annotation required
  e: function(arg) {  // missing arg and return annotations
    return {
      bar: arg
    };
  },

  g: Foo.h('bar'),            // no annotation required
  h: function(arg: string) {  // missing return annotation
    return {
      bar: arg
    };
  },

  i: Foo.j('bar'),
  j: function(arg: string): {
    bar: string
  } {
    return {
      bar: arg
    };
  },
};

module.exports = Foo;
