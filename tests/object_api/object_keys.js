/* @flow */

var sealed = {one: 'one', two: 'two'};
(Object.keys(sealed): Array<'one'|'two'>);
(Object.keys(sealed): void); // error, Array<string>

var unsealed = {};
Object.keys(unsealed).forEach(k => {
  (k : number) // error: number ~> string
});

var dict: { [k: number]: string } = {};
Object.keys(dict).forEach(k => {
  (k : number) // error: number ~> string
});

var any: Object = {};
(Object.keys(any): Array<number>); // error, Array<string>
