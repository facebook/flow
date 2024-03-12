interface I {
  foo: number;
}

type MappedInterface = {+[key in keyof I]: I[key]};
{
  declare const i: MappedInterface;
  (i: interface {+foo: number}); // OK
  (i: interface {foo: number}); // ERROR
  (i: {+foo: number, ...}); // ERROR (class object subtyping)
}

interface WithIndexer {
  foo: number;
  [string]: boolean;
}
type MappedInterfaceWithIndexer = {+[key in keyof WithIndexer]: WithIndexer[key]};
{
  declare const i: MappedInterfaceWithIndexer;
  (i: interface {+foo: number, +[string]: boolean}); // OK
  (i: interface {+foo: number, [string]: boolean}); // ERROR
}

class A {
  static bar: number;
  foo: number;
}

type MappedInstance = {+[key in keyof A]: A[key]};
{
  declare const inst: MappedInstance;
  (inst: interface {+foo: number}); // OK
  (inst: interface {foo: number}); // ERROR
  (inst['bar']); // ERROR
}

type MappedClass = {+[key in keyof Class<A>]: Class<A>[key]};
{
  declare const c: MappedClass;
  (c: interface {+bar: number}); // OK
  (c: interface {bar: number}); // ERROR
}
