// @flow

declare class Box<A, B, C> {
  same: (value: C) => A;
}
interface Box<X, Y, Z> {
  same: (value: Z) => X;
  extra: Y;
}

declare const box: Box<number, string, boolean>;
box.extra as string;
box.extra as number; // ERROR
box.same(true) as number;
box.same(true) as string; // ERROR
box.same(0); // ERROR

interface Pair<A, B> {
  same: (value: B) => A;
}
interface Pair<X, Y> {
  same: (value: Y) => X;
  second: Y;
}

declare const pair: Pair<number, string>;
pair.second as string;
pair.second as number; // ERROR
pair.same("") as number;
pair.same("") as string; // ERROR
pair.same(0); // ERROR

declare class SameClassDefault<T = string> {}
interface SameClassDefault<U = string> {}

interface SameReverseClassDefault<T = string> {}
declare class SameReverseClassDefault<U = string> {}

declare class ConflictingClassDefault<T = string> {}
interface ConflictingClassDefault<U = number> {} // ERROR
