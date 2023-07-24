// @flow

// Variable defs and uses
var foo = function() { };
foo();
foo = null;
foo();

// Type aliases
type T = number;
type S = T;
((_: T) => {});

// Destructuring
let { x, y } = { x: 0, y: 0 };
let { x: _x, y: _y } = { x, y };
({ x: _x, y: _y });

let { [x]: y } = { };

// Imports
import { wut3 } from 'wutland';
import type { wut4 } from 'wutland';
(wut3: wut4);
import { type wut5 } from 'wutland';
import { type wut6 as wutLocal } from 'wutland';
(wut: wut5);
(wut: wutLocal);

// Exports
const localVar = 1;
const aliasedVar = 1;
export {localVar}
export {aliasedVar as newAlias}

type Props = {foo: string};
function MyComponent({foo}: Props) {
  return <div />;
}
<MyComponent foo={42} />;
