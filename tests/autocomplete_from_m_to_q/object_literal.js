// @flow

type T = {foo: string, bar: number};

const x: T = {  };
//             ^
const y: T = {    : "foo" };
//              ^

declare function f(x: {foo?: string, bar: number, baz?: boolean}): void;
f({
         // Shoud suggest `foo` and `baz`
// ^
  bar: 1,
         // Shoud suggest `foo` and `baz`
// ^
});

const withoutHint = {
  existing: {nested: 1},
    
// ^
};

class Prototype {
  method(): number { return 1; }
}

const inherited = {
  __proto__: new Prototype(),
    
// ^
};
