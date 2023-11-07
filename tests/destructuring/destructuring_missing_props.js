//@flow
declare var x: {|foo: string|};
var {nonExistent, defaults = 'hi', foo = 3} = x; // Error, missing nonExistent
defaults as string;
foo as number; // Error, number | string
foo as number | string;

declare var y: {|bar: string|};
var {nonExistent2, defaults2 = 'hi', bar = 3} = y; // Error, missing nonExistent2
defaults2 as string;
bar as string; // Error, number | string
bar as number | string;

var {baz = 15150} = null; // Error, baz is missing in null (you can't destructure null)

declare var z: {thud: string};
var {grunt = 15210} = z; // Error, grunt missing in inexact object type

const proto: {|foo: number|} = {foo: 3};
const obj = {__proto__: proto, baz: 'string'};
var {qux = 'string'} = obj; // Error, qux missing

// Begin React examples

const React = require('react');
function Component({
  defaultProps = 'default',
  regularProp,
}: {|
  regularProp?: number,
|}) {
  // Error, missing regularProp
  defaultProps as string;
  regularProp as number;
  return null;
}

const _a = <Component regularProp={3} />;
const _b = <Component />;

class A {
  prop: boolean;
  // No err! prop will always be initialized to a boolean
  constructor({prop = false}: {|prop?: boolean|} = {...null}) {
    prop as boolean; // OK
  }
}
