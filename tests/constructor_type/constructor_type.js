// Basic constructor type
type Ctor = new () => {x: number};
declare const c: Ctor;
const inst = new c();
inst.x as number; // OK

// Constructor type with params
type CtorWithArgs = new (x: number) => {v: string};
declare const c2: CtorWithArgs;
new c2(1); // OK
new c2("bad"); // ERROR: string incompatible with number

// Assignability with interface construct signature
interface ICtor { new (): {x: number} }
declare const ic: ICtor;
ic as Ctor; // should be compatible

// Non-constructable interface should still error
interface NotConstructable {
    bar(): number;
}
declare const nc: NotConstructable;
new nc(); // ERROR: invalid-constructor
