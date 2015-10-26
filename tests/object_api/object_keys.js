/* @flow */

var sealed = {one: 'one', two: 'two'};
var keys : Array<'one'|'two'> = Object.keys(sealed);

var unsealed = {};
Object.keys(unsealed).forEach(k => {
  (k : number) // error: number ~> string
});

var dict: { [k: number]: string } = {};
Object.keys(dict).forEach(k => {
  (k : number) // error: number ~> string
});
