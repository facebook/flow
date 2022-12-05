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

(Object.entries(dict): Array<[string, number]>); // OK
(Object.entries(iface): Array<[string, mixed]>); // OK
(Object.entries(dictWithProps): Array<[string, mixed]>); // OK
(Object.entries(dictUnion): Array<[string, number | boolean]>); // OK
(Object.entries(writeOnlyDict): Array<[string, mixed]>); // OK
(Object.entries(obj): Array<['a' | 'b', mixed]>); // OK
(Object.entries(instance): Array<['a' | 'b', mixed]>); // OK

(Object.entries(dict): Array<[string, empty]>); // ERROR
(Object.entries(iface): Array<[string, empty]>); // ERROR
(Object.entries(dictWithProps): Array<[string, empty]>); // ERROR
(Object.entries(dictUnion): Array<[string, empty]>); // ERROR
(Object.entries(writeOnlyDict): Array<[string, empty]>); // ERROR
(Object.entries(obj): Array<['a' | 'b', empty]>); // ERROR
(Object.entries(instance): Array<['a' | 'b', empty]>); // ERROR
(Object.entries(dict): Array<empty>); // ERROR

// Invalid inputs
Object.entries(undefined); // ERROR
Object.entries(null); // ERROR
Object.entries(1); // ERROR
Object.entries(true); // ERROR
Object.entries([1, 2]); // ERROR
