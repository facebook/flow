declare const key: number;

declare const roArray: $ReadOnlyArray<string>;
roArray[0] as string; // error: void ~> string
roArray[key] as string; // error: void ~> string
declare const rwArray: Array<string>;
rwArray[0] as string; // error: void ~> string
rwArray[key] as string; // error: void ~> string
rwArray[0] = undefined; // error: void ~> string
declare const tuple: [0, 1, 2];
tuple[0] as 0;
tuple[1] as 1;
tuple[2] as 2;
tuple[3] as number; // error: out of bound
tuple[key] as number; // error: void ~> number

declare export const typeTest: (typeof roArray)[number];
typeTest as string; // ok: the flag should not affect type-level access
