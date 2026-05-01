declare var dict: {[string]: number};

declare var dictWithProps: {[string]: number, a: boolean};

declare var writeOnlyDict: {-[string]: number};

declare var dictUnion: {[string]: number} | {[string]: boolean};

declare var obj: {a: 1, b: 2};

declare class C {
  a: number;
  b: string;
}
declare var instance: C;

declare var iface: interface {[string]: number};

Object.values(dict) as Array<number>; // OK
Object.values(iface) as Array<mixed>; // OK
Object.values(dictWithProps) as Array<mixed>; // OK
Object.values(dictUnion) as Array<number | boolean>; // OK
Object.values(writeOnlyDict) as Array<mixed>; // OK
Object.values(obj) as Array<mixed>; // OK
Object.values(instance) as Array<mixed>; // OK

Object.values(dict) as Array<empty>; // ERROR
Object.values(iface) as Array<empty>; // ERROR
Object.values(dictWithProps) as Array<empty>; // ERROR
Object.values(dictUnion) as Array<empty>; // ERROR
Object.values(writeOnlyDict) as Array<empty>; // ERROR
Object.values(obj) as Array<empty>; // ERROR
Object.values(instance) as Array<empty>; // ERROR

// Invalid inputs
Object.values(undefined); // ERROR
Object.values(null); // ERROR
Object.values(1); // ERROR
Object.values(true); // ERROR
Object.values([1, 2]); // ERROR
