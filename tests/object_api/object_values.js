/* @flow */

// Works for known-types
const a = { first: 1, second: 2 };
(Object.values(a): Array<number>);

const b = { first: 1, second: 2, last: 'different' };
(Object.values(b): Array<mixed>); // mixed if the keys aren't all the same
(Object.values(b): Array<number>); // error

const c = {};
(Object.values(c): Array<empty>);

const d = {};
d.first = 'test';
d.second = 'another test';

(Object.values(d): Array<string>);
(Object.values(d): Array<number>); // error

d.something = true;
(Object.values(d): Array<mixed>);
(Object.values(d): Array<string>); // error

