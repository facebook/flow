type A = {x?: number, y?: number};

({}) as A; // OK
({x: 1}) as A; // OK
({y: 1}) as A; // OK
({x: 1, y: 1}) as A; // OK

// Required
({}) as Required<A>; // ERROR
({x: 1}) as Required<A>; // ERROR
({y: 1}) as Required<A>; // ERROR
({x: undefined, y: undefined}) as Required<A>; // ERROR
({x: 1, y: 1, extra: 1}) as Required<A>; // ERROR

({x: 1, y: 1}) as Required<A>; // OK

// Void input
undefined as Required<void>; // OK
null as Required<void>; // ERROR

// Null input
null as Required<null>; // OK
undefined as Required<null>; // ERROR

// Nullable input
({}) as Required<?A>; // ERROR
({x: 1}) as Required<?A>; // ERROR
({y: 1}) as Required<?A>; // ERROR
({x: undefined, y: undefined}) as Required<?A>; // ERROR

undefined as Required<?A>; // OK
null as Required<?A>; // OK
({x: 1, y: 1}) as Required<?A>; // OK

// Invalid
type Err = Required<number>; // ERROR
1 as Err;

// Interface
interface IFace {
  x?: number;
  y?: number;
}

({}) as IFace; // OK
({x: 1}) as IFace; // OK
({y: 1}) as IFace; // OK
({x: 1, y: 1}) as IFace; // OK
({x: 1, y: 1, extra: 1}) as IFace; // OK

({}) as Required<IFace>; // ERROR
({x: 1}) as Required<IFace>; // ERROR
({y: 1}) as Required<IFace>; // ERROR
({x: undefined, y: undefined}) as Required<IFace>; // ERROR

({x: 1, y: 1}) as Required<IFace>; // OK
({x: 1, y: 1, z: 1}) as Required<IFace>; // OK

interface JFace extends IFace {
  z?: number;
}

({}) as JFace; // OK
({x: 1}) as JFace; // OK
({x: 1, y: 1}) as JFace; // OK
({x: 1, y: 1, z: 1}) as JFace; // OK

({}) as Required<JFace>; // ERROR
({x: 1}) as Required<JFace>; // ERROR
({x: 1, y: 1}) as Required<JFace>; // ERROR
({x: undefined, y: undefined, z: undefined}) as Required<JFace>; // ERROR

({x: 1, y: 1, z: 1}) as Required<JFace>; // OK

// No change
({x: 1, y: 1}) as Required<{x: number, y: number}>; // OK
