import type {T, S, ReadonlyValues} from './array';

declare const x: T;
x as S; // OK
x.push("a"); // ERROR
x[0] = "b"; // ERROR
x[0] as string; // OK

declare const y: S;
y as T; // OK

declare const generic: ReadonlyValues<number>;
generic as ReadonlyArray<number>; // OK
