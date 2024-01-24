import {FooClassExported, FooClassStringExported, barFExported, barGenericExported, EscapedGenericExported} from './with-targs-exported';

class Foo<T> {}
declare const FooClass: typeof Foo<string>;

declare function bar<T>(): T;
declare const barF: typeof bar<string>;

new FooClass() as Foo<number>; // error: string ~> number
barF() as number; // error: string ~> number
new FooClassStringExported() as FooClassExported<number>; // error: string ~> number
barFExported() as number; // error: string ~> number

type Invalid1 = typeof barF<string>; // error
type Invalid2 = typeof FooClass<string>; // error
type Invalid3 = typeof barFExported<string>; // error
type Invalid4 = typeof FooClassStringExported<string>; // error
type Invalid5 = typeof bar<string, string>;
type Invalid6 = typeof Foo<string, string>;

declare var emptyValue: empty;
emptyValue as Invalid1;
emptyValue as Invalid2;
emptyValue as Invalid3;
emptyValue as Invalid4;
emptyValue as Invalid5; // error
emptyValue as Invalid6; // error

declare function barGeneric<X>(cb: typeof bar<X>): X;

barGeneric<number>(() => 1); // ok
barGeneric<string>(() => 1); // error: number ~> string
barGeneric(() => 1) as number; // ok
barGeneric(() => 1) as string; // error: number ~> string
barGenericExported<number>(() => 1); // ok
barGenericExported<string>(() => 1); // error: number ~> string
barGenericExported(() => 1) as number; // ok
barGenericExported(() => 1) as string; // error: number ~> string

class EscapedGeneric<T> {
  static x: T;
}
emptyValue as typeof EscapedGeneric.x<number, boolean>; // error: empty not polymorphic
emptyValue as typeof EscapedGenericExported.x<number, boolean>; // error: empty not polymorphic
