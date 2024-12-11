declare const recordObj: {foo: string};
declare const dictObj: {[string]: string};
declare const key: string;

recordObj[key] as string; // error: bad key
dictObj[key] as string; // error: void ~> string
dictObj.hahaha as string; // error: void ~> string

declare export const typeTest: (typeof dictObj)[string];
typeTest as string; // ok: the flag should not affect type-level acccess
