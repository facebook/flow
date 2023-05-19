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

// Imports
import {a} from './to_be_imported';
a;
const A = require('./to_be_imported');
A.a;
const AA = {BB:A};
AA.BB.a;
