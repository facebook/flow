interface I {
  foo: number;
}

type MappedInterface = {+[key in keyof I]: I[key]};
{
  declare const i: MappedInterface;
  i as interface {+foo: number}; // OK
  i as interface {foo: number}; // ERROR
  i as {+foo: number, ...}; // ERROR (class object subtyping)
}

interface WithIndexer {
  foo: number;
  [string]: boolean;
}
type MappedInterfaceWithIndexer = {+[key in keyof WithIndexer]: WithIndexer[key]};
{
  declare const i: MappedInterfaceWithIndexer;
  i as interface {+foo: number, +[string]: boolean}; // OK
  i as interface {+foo: number, [string]: boolean}; // ERROR
}

class A {
  static bar: number;
  foo: number;
}

type MappedInstance = {+[key in keyof A]: A[key]};
{
  declare const inst: MappedInstance;
  inst as interface {+foo: number}; // OK
  inst as interface {foo: number}; // ERROR
  (inst['bar']); // ERROR
}

type MappedClass = {+[key in keyof Class<A>]: Class<A>[key]};
{
  declare const c: MappedClass;
  c as interface {+bar: number}; // OK
  c as interface {bar: number}; // ERROR
}
