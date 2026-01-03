declare function f<T: string>(t: T): void;

declare const v1: typeof f extends (v: unknown) => void ? true : false;
v1 as false; // ok
v1 as empty; // error: false ~> empty

declare const v2: typeof f extends (v: string) => void ? true : false;
v2 as true; // ok
v2 as empty; // error: true ~> empty

declare const v3: typeof f extends (v: number) => void ? true : false;
v3 as false; // ok
v3 as empty; // error: false ~> empty

declare const v4: typeof f extends (v: infer V) => void ? V : false;
v4 as string; // ok
v4 as empty; // error: string ~> empty
