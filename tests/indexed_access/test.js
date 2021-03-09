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
