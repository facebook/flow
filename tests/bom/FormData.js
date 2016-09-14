/* @flow */

// constructor
const a: FormData = new FormData(); // correct
new FormData(''); // incorrect
new FormData(document.createElement('input')); // incorrect
new FormData(document.createElement('form')); // correct

// has
const b: boolean = a.has('foo'); // correct

// get
const c: ?string = a.get('foo'); // correct
const d: string = a.get('foo'); // incorrect
a.get(2); // incorrect

// getAll
a.getAll('foo').map((x: string) => x); // correct
a.getAll('foo').map((x: number) => x); // incorrect
a.getAll(23); // incorrect

// set
a.set('foo', 'bar'); // correct
a.set('foo', {}); // incorrect
a.set(2, 'bar'); // incorrect
a.set('foo', 'bar', 'baz'); // correct
a.set('foo', 'bar', {}); // incorrect

// append
a.append('foo', 'bar'); // correct
a.append('foo', {}); // incorrect
a.append(2, 'bar'); // incorrect
a.append('foo', 'bar', 'baz'); // correct
a.append('foo', 'bar', {}); // incorrect

// delete
a.delete('xx'); // correct
a.delete(3); // incorrect

// keys
for (let x: string of a.keys()) {} // correct
for (let x: number of a.keys()) {} // incorrect

// values
for (let x: string of a.values()) {} // correct
for (let x: number of a.values()) {} // incorrect

// entries
for (let [x, y]: [string, string] of a.entries()) {} // correct
for (let [x, y]: [number, string] of a.entries()) {} // incorrect
for (let [x, y]: [string, number] of a.entries()) {} // incorrect
for (let [x, y]: [number, number] of a.entries()) {} // incorrect
