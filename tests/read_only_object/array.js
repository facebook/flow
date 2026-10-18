export type T = Readonly<Array<string>>;
export type S = ReadonlyArray<string>;

declare const x: T;
x as S; // OK
x[0] as string; // OK
x[0] as number; // ERROR
x.push("a"); // ERROR
x[0] = "b"; // ERROR
x as Array<string>; // ERROR

declare const y: S;
y as T; // OK

declare const mutable: Array<string>;
mutable as T; // OK

{
  declare const x: Readonly<string[]>;
  x as S; // OK
  y as Readonly<string[]>; // OK
}

{
  declare const x: Readonly<ReadonlyArray<string>>;
  x as S; // OK
  y as Readonly<ReadonlyArray<string>>; // OK
  x.push("a"); // ERROR
  x[0] = "b"; // ERROR
}

{
  declare const x: Readonly<T>;
  x as S; // OK
  y as Readonly<T>; // OK
}

export type ReadonlyValues<U> = Readonly<Array<U>>;
function generic<U>(x: ReadonlyValues<U>, y: ReadonlyArray<U>): void {
  x as ReadonlyArray<U>; // OK
  y as ReadonlyValues<U>; // OK
  x as Array<U>; // ERROR
  x[0] as U; // OK
}

{
  declare const x: ReadonlyValues<string>;
  x as ReadonlyArray<string | number>; // OK
  x as ReadonlyValues<number>; // ERROR
}

{
  declare const x: Readonly<Array<string> | Array<number>>;
  x as ReadonlyArray<string> | ReadonlyArray<number>; // OK
  x[0] as string | number; // OK
  x[0] = "a"; // ERROR

  declare const y: ReadonlyArray<string> | ReadonlyArray<number>;
  y as Readonly<Array<string> | Array<number>>; // OK
}

{
  declare const x: Readonly<Array<{value: string}>>;
  x[0].value = "a"; // OK: Readonly is shallow
  x[0] = {value: "b"}; // ERROR
}

{
  declare const x: Readonly<Array<Array<string>>>;
  x[0].push("a"); // OK: Readonly is shallow
  x[0][0] = "b"; // OK
  x.push([]); // ERROR
}
