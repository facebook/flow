/* @flow */

var Foo = {
  a: function(arg) {  // missing arg and return annotations
    return arg;
  },

  b: function(arg) {  // missing arg and return annotations
    return {
      bar: arg
    };
  },

  c: function(arg: string) {  // missing return annotation
    return {
      bar: arg
    };
  },

  d: function(arg: string): {
    bar: string
  } {
    return {
      bar: arg
    };
  },
};

var Bar = {
  a: Foo.a('Foo'),    // no annotation required

  // object property types are inferred, so make sure that this doesn't cause
  // us to also infer the parameter's type.
  b: Foo.b('bar'),    // no annotation required

  c: Foo.c('bar'),            // no annotation required

  d: Foo.d('bar'),            // no annotation required
};

module.exports = Foo, Bar;
