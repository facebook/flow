/**
 * @flow
 */

// progressively annotate:

//function f(x) { return x; }
function f(x:number): number { return x; }
//function f(x:number):string { return x; }

var x:string = f(0);

module.exports = f;
