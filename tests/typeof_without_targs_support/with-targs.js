import {FooClassExported, FooClassStringExported, barFExported, barGenericExported} from './with-targs-exported';

class Foo<T> {}
declare const FooClass: typeof Foo<string>; // unsupported

declare function bar<T>(): T;
declare const barF: typeof bar<string>; // unsupported

new FooClass() as Foo<number>; // error: string ~> number
barF() as number; // error: string ~> number
new FooClassStringExported() as FooClassExported<number>; // error: string ~> number
barFExported() as number; // error: string ~> number

type Invalid1 = typeof barF<string>; // unsupported
type Invalid2 = typeof FooClass<string>; // unsupported
type Invalid3 = typeof barFExported<string>; // unsupported
type Invalid4 = typeof FooClassStringExported<string>; // unsupported
type Invalid5 = typeof bar<string, string>; // unsupported
type Invalid6 = typeof Foo<string, string>; // unsupported

declare var emptyValue: empty;
emptyValue as Invalid1;
emptyValue as Invalid2;
emptyValue as Invalid3;
emptyValue as Invalid4;
emptyValue as Invalid5;
emptyValue as Invalid6;

declare function barGeneric<X>(cb: typeof bar<X>): X; // unsupported

barGeneric<number>(() => 1); // error: number ~> GenericT
barGeneric<string>(() => 1); // error: number ~> GenericT
barGenericExported<number>(() => 1); // error: number ~> GenericT
barGenericExported<string>(() => 1); // error: number ~> GenericT
