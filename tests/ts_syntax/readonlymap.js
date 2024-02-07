type T = ReadonlyMap<string, number>; // ERROR
const x: T = 3; // ok, since T interpreted to be any when experimental.ts_syntax is off
