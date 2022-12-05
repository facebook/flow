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

(Object.values(dict): Array<number>); // OK
(Object.values(iface): Array<mixed>); // OK
(Object.values(dictWithProps): Array<mixed>); // OK
(Object.values(dictUnion): Array<number | boolean>); // OK
(Object.values(writeOnlyDict): Array<mixed>); // OK
(Object.values(obj): Array<mixed>); // OK
(Object.values(instance): Array<mixed>); // OK

(Object.values(dict): Array<empty>); // ERROR
(Object.values(iface): Array<empty>); // ERROR
(Object.values(dictWithProps): Array<empty>); // ERROR
(Object.values(dictUnion): Array<empty>); // ERROR
(Object.values(writeOnlyDict): Array<empty>); // ERROR
(Object.values(obj): Array<empty>); // ERROR
(Object.values(instance): Array<empty>); // ERROR

// Invalid inputs
Object.values(undefined); // ERROR
Object.values(null); // ERROR
Object.values(1); // ERROR
Object.values(true); // ERROR
Object.values([1, 2]); // ERROR
