// @flow

// Variable defs and uses
var foo = function() { };
foo();
foo = null;
foo();

// Nested functions
function bar() {
  function bar() {
  }
  bar();
}
bar();

// Classes
class C { }
new C;
class D extends C { }

// Type aliases
type T = number;
type S = T;
((_: T) => {});

// Refinements
let nullable: ?string = "";
if (nullable != null) {
  console.log(nullable.length);
  nullable = null;
}
(nullable: null);

// Destructuring
let { x, y } = { x: 0, y: 0 };
let { x: _x, y: _y } = { x, y };
({ x: _x, y: _y });

let { [x]: y } = { };

// Not in scope
wut1;
wut1 = wut2;
wut2;

// JSX
var React = require('react');
class Fancy extends React.Component {
  props: { x: number };
}
<Fancy x=0/>;

// Imports
import { wut3 } from 'wutland';
import type { wut4 } from 'wutland';
(wut3: wut4);

// Qualified types
(null: React.Component);

// Exports
export const exportedConst = 5;
export function exportedFunction(): void {}

console.log(exportedConst);
exportedFunction();

import {ExternalClass} from './external';

class SuperClass {
  bar(): void {}
  baz: string;
}

class SubClass extends SuperClass {}

const superClass = new SuperClass();
const subClass = new SubClass();
const externalClass = new ExternalClass();

superClass.bar();
superClass.bar();
superClass.baz;
superClass.baz;

subClass.bar();
subClass.bar();
subClass.baz;
subClass.baz;

externalClass.bar();
externalClass.bar();

class WithTypeParams<X, Y> {
  bar(): void { }
  baz(): void {
    this.bar();
  }
}
