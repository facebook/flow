// @flow

Object.assign({}, { foo(x) {} });  // error missing annotation
Object.create({ foo(x) {} });  // error missing annotation
