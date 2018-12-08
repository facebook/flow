/* @flow */

// argument doesn't have to be an object
(Object.entries(1): Array<[string, mixed]>);
(Object.entries(false): Array<[string, mixed]>);
(Object.entries("blah"): Array<[string, mixed]>);

// sealed objects have more specific types
const sealed = { foo: 'foo', bar: 2 };
(Object.entries(sealed): Array<[string, string|number]>);

// objects-as-maps have more specific types
const strToNum: {[string]: number} = {};
(Object.entries(strToNum): Array<[string, number]>);

const strToStr: {[string]: string} = {};
(Object.entries(strToStr): Array<[string, string]>);

const numToNum: {[number]: number} = {};
(Object.entries(numToNum): Array<[string, number]>);

Object.entries(strToNum).forEach(([key, val]) => {
    (val: string) // error: number ~> string
});
