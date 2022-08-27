//@flow

declare var obj1: { f: number };
delete obj1.f; // error, f is required


declare var obj2: { f?: number };
delete obj2.f; //fine

var x: number | void = 42;
delete x;
(x: number);
(x: number | void);

var y: number = 42;
delete y;

type A = { -a?: string };

declare var a: A;


const z = delete a.a;


declare var b: $ReadOnlyArray<number>;
delete b[0];

declare var c: [number, number];
delete c[1];
delete c[2];

var w: {a: number | void} = {a: 42};
(w.a: void);

var w2: {a: number | void} = {a: 42};
delete w2.a;
(w2.a: void);

const obj4 = Object.freeze({ f: 1 });
delete obj4.f; // error, just like when writing to frozen object

declare var obj5: { +f?: number };
delete obj5.f; // error, just like when writing to read-only object

class C { x: void; m() {} }
declare var obj6: C;
delete obj6.x;
delete obj6.m; // warn, m is not own (delete only has effect
               // on own-properties)

delete 1;
delete C;
delete (() => 42);


declare var f: Array<string>;
delete f[0];

declare var index: { [string]: number, a: boolean };
delete index.a;
delete index['a'];
delete index.b;
delete index['foo'];
