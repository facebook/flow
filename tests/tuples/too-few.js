/* @flow */

function foo(a: [any, any]) {}

foo([ {} ]); // error, too few elements in array passed to a tuple
