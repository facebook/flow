/* @flow */

// argument doesn't have to be an object
(Object.values(1): Array<mixed>);
(Object.values(false): Array<mixed>);
(Object.values("blah"): Array<mixed>);

// sealed objects have more specific types
const sealed = { foo: 'foo', bar: 2 };
(Object.values(sealed): Array<string|number>);

// objects-as-maps have more specific types
const strToNum: {[string]: number} = {};
(Object.values(strToNum): Array<number>);

const strToStr: {[string]: string} = {};
(Object.values(strToStr): Array<string>);

const numToNum: {[number]: number} = {};
(Object.values(numToNum): Array<number>);

Object.values(strToNum).forEach(val => {
    (val: string) // error: number ~> string
});
