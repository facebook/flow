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

Object.entries(dict) as Array<[string, number]>; // OK
Object.entries(iface) as Array<[string, unknown]>; // OK
Object.entries(dictWithProps) as Array<[string, unknown]>; // OK
Object.entries(dictUnion) as Array<[string, number | boolean]>; // OK
Object.entries(writeOnlyDict) as Array<[string, unknown]>; // OK
Object.entries(obj) as Array<['a' | 'b', unknown]>; // OK
Object.entries(instance) as Array<['a' | 'b', unknown]>; // OK

Object.entries(dict) as Array<[string, empty]>; // ERROR
Object.entries(iface) as Array<[string, empty]>; // ERROR
Object.entries(dictWithProps) as Array<[string, empty]>; // ERROR
Object.entries(dictUnion) as Array<[string, empty]>; // ERROR
Object.entries(writeOnlyDict) as Array<[string, empty]>; // ERROR
Object.entries(obj) as Array<['a' | 'b', empty]>; // ERROR
Object.entries(instance) as Array<['a' | 'b', empty]>; // ERROR
Object.entries(dict) as Array<empty>; // ERROR

// Invalid inputs
Object.entries(undefined); // ERROR
Object.entries(null); // ERROR
Object.entries(1); // ERROR
Object.entries(true); // ERROR
Object.entries([1, 2]); // ERROR
