// @flow

type T = {| foo: string, bar: number |};

const x: T = {  };
//            ^^
const y: T = {    : "foo" };
//             ^^
