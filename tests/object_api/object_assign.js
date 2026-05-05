/* @flow */

// $FlowExpectedError[unsafe-object-assign]
var export_ = Object.assign({} as {foo?: unknown => unknown}, {
  foo: function (param: unknown) {
    return param;
  },
});

// $FlowExpectedError[unsafe-object-assign]
var decl_export_: {foo: any, bar: any} = Object.assign(
  {} as {foo?: unknown => unknown, bar?: any},
  export_,
);

let anyObj: Object = {};
// $FlowExpectedError[unsafe-object-assign]
Object.assign(anyObj, anyObj); // makes sure this terminates
