/* @flow */

const a = { first: 'test', second: 'thing' };
(Object.entries(a): Array<[string, string]>);
(Object.entries(a): Array<[string, number]>); // error

const b = { first: 1, second: true, third: 'test' };
(Object.entries(b): Array<[string, mixed]>);
(Object.entries(b): Array<[string, string]>); // error
