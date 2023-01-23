// @flow

type Foo = 'foo';

const a: Foo = ;
//             ^
const b: Foo = '';
//              ^
const c: Foo = ';
//              ^
const d: Foo = f;
//              ^
const e: Foo = 'f';
//               ^
const f: Foo = 'f;
//               ^
const g: Foo = "";
//              ^
const h: Foo = ";
//              ^
const i: Foo = "f";
//               ^
const j: Foo = "f;
//               ^
const k: Foo = 'far';
//               ^
const l: Foo = "far";
//               ^
