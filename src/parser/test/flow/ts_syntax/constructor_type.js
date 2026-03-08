type T = new () => HTMLElement;
type U = new (x: string) => Foo;
type V = new (...args: any[]) => any;
type W = new <T>(x: T) => T;
declare const Chalk: new (options?: string) => string;
type A = abstract new () => HTMLElement;
type B = abstract new (x: string) => Foo;
type C = abstract new (...args: any[]) => any;
type D = abstract new <T>(x: T) => T;
type DrizzleEntityClass<T> = ((abstract new (...args: any[]) => T) | (new (...args: any[]) => T)) & DrizzleEntity;
type NewAsIdentifier = new;
type Nested = new () => new () => string;
type ThisParam = new (this: string) => void; // error: this param banned
