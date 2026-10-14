// The rest of an exact tuple has the element type of the remaining elements only.

declare const t: [number, string];
const [first, ...rest] = t;
rest as [string]; // ok
rest as ReadonlyArray<string>; // ok
rest as ReadonlyArray<number>; // error: string ~> number

declare function after_string<T extends ReadonlyArray<boolean>>(
  f: (...args: [string, ...T]) => void,
): T;
declare function string_boolean(a: string, b: boolean): void;
after_string(string_boolean) as [boolean]; // ok

declare const inexact: [number, string, ...];
const [_, ...inexact_rest] = inexact;
inexact_rest as ReadonlyArray<string>; // error: unknown element ~> string
