type Arr = Array<number>;
type ArrElem = Arr[number];

(42: ArrElem); // OK
('hello world': ArrElem); // Error

function foo(a: Arr): Arr[number] {
  if (false) return a[0];
  else return 0;
}

type Dict = { [key: string]: number };
type DictElem = Dict[string];

(42: DictElem); // OK
('hello world': DictElem); // Error

function bar(o: Dict): Dict[string] {
  if (false) return o['buz'];
  else return 0;
}

type Obj = {
  foo: boolean,
  baz: string,
  ...
};

export type ObjFoo = Obj['foo'];
(true: ObjFoo); // OK
(1: ObjFoo); // Error

('hi': Obj['baz']); // OK
(1: Obj['baz']); // Error

function f<To: Obj>(o: To): To['foo'] {
  return o.foo;
}
declare var o: Obj;
(f(o): boolean); // OK
(f({foo: true, baz: 'hi', bort: 1}): boolean); // OK
(f(o): string); // Error

type Nonexistant = Obj['bork']; // Error
(1: Nonexistant);

type ArrNonexistant = Arr[boolean]; // Error
(1: ArrNonexistant);

(1: void['x']); // Error
(1: null['x']); // Error

type Bar = 'bar';

type O = {
  [number]: boolean;
  bar: number;
}
(1: O['bar']); // OK
(1: O[Bar]); // OK
(true: O[number]); // OK
('xx': O['bar']); // Error

declare class C {
  [number]: boolean;
  bar: number;
}
(1: C['bar']); // OK
(1: C[Bar]); // Should be ok, currently errors
(true: C[number]); // OK
('xx': C['bar']); // Error

interface I {
  [number]: boolean;
  bar: number;
}
(1: I['bar']); // OK
(1: I[Bar]); // Should be ok, currently errors
(true: I[number]); // OK
('xx': I['bar']); // ERROR

interface M {
  method(): number;
}
(() => 1) as M['method']; // OK
(() => true) as M['method']; // ERROR

{
  const test = (f: M['method']) => {
    f() as number; // OK
    f() as empty; // ERROR
  }
}
