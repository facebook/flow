// `new` evaluates to the constructor's return when it is object-like, and to
// the instance otherwise

declare class MaybeObject {
  constructor(): {x: number} | void;
  m: number;
}
new MaybeObject() as {x: number} | MaybeObject;
new MaybeObject() as number; // error x2

declare class AnyReturn {
  constructor(): any;
}
new AnyReturn() as number;

// The first intersection member decides
declare class Intersection {
  constructor(): {x: number} & {z: string};
}
new Intersection() as {x: number};
new Intersection() as {x: number, z: string, ...}; // error
new Intersection() as number; // error

declare class NumberFirst {
  constructor(): number & {x: number};
}
new NumberFirst() as NumberFirst;
new NumberFirst() as number; // error

declare class FunctionReturn {
  constructor(): () => void;
}
new FunctionReturn() as () => void;
new FunctionReturn() as number; // error

declare class StringReturn {
  constructor(): string;
}
new StringReturn() as StringReturn;
new StringReturn() as string; // error

declare class Generic<T> {
  constructor(): T;
}
new Generic<{q: number}>() as {q: number};
new Generic<string>() as Generic<string>;

// The constructor's return doesn't constrain implicitly instantiated type arguments
declare class Inferred<T> {
  constructor(f: (x: T) => void): T;
}
const inferred = new Inferred((x: string) => {});
inferred as Inferred<string>;
declare class Defaulted<T = string> {
  constructor(): T;
}
const defaulted = new Defaulted();
defaulted as Defaulted<string>;
declare class Contravariant<in T> {
  constructor(x: T): T;
  m(x: T): void;
}
const contravariant = new Contravariant(1);
contravariant as Contravariant<number>;
declare class MaybeInferred<T> {
  constructor(f: (x: T) => void): ?T;
}
const maybeInferred = new MaybeInferred((x: string) => {});
maybeInferred as MaybeInferred<string>;
