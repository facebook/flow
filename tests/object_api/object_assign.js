/* @flow */

var export_ = Object.assign({} as {foo?: mixed => mixed}, {
  foo: function (param: mixed) {
    return param;
  },
});

var decl_export_: {foo: any, bar: any} = Object.assign(
  {} as {foo?: mixed => mixed, bar?: any},
  export_,
);

let anyObj: Object = {};
Object.assign(anyObj, anyObj); // makes sure this terminates
