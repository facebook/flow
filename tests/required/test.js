type A = {x?: number, y?: number};

({}: A); // OK
({x: 1}: A); // OK
({y: 1}: A); // OK
({x: 1, y: 1}: A); // OK

// Required
({}: $Required<A>); // ERROR
({x: 1}: $Required<A>); // ERROR
({y: 1}: $Required<A>); // ERROR
({x: undefined, y: undefined}: $Required<A>); // ERROR
({x: 1, y: 1, extra: 1}: $Required<A>); // ERROR

({x: 1, y: 1}: $Required<A>); // OK

// Void input
(undefined: $Required<void>); // OK
(null: $Required<void>); // ERROR

// Null input
(null: $Required<null>); // OK
(undefined: $Required<null>); // ERROR

// Nullable input
({}: $Required<?A>); // ERROR
({x: 1}: $Required<?A>); // ERROR
({y: 1}: $Required<?A>); // ERROR
({x: undefined, y: undefined}: $Required<?A>); // ERROR

(undefined: $Required<?A>); // OK
(null: $Required<?A>); // OK
({x: 1, y: 1}: $Required<?A>); // OK

// Invalid
type Err = $Required<number>; // ERROR
(1: Err);

// Interface
interface IFace {
  x?: number;
  y?: number;
}

({}: IFace); // OK
({x: 1}: IFace); // OK
({y: 1}: IFace); // OK
({x: 1, y: 1}: IFace); // OK
({x: 1, y: 1, extra: 1}: IFace); // OK

({}: $Required<IFace>); // ERROR
({x: 1}: $Required<IFace>); // ERROR
({y: 1}: $Required<IFace>); // ERROR
({x: undefined, y: undefined}: $Required<IFace>); // ERROR

({x: 1, y: 1}: $Required<IFace>); // OK
({x: 1, y: 1, z: 1}: $Required<IFace>); // OK

interface JFace extends IFace {
  z?: number;
}

({}: JFace); // OK
({x: 1}: JFace); // OK
({x: 1, y: 1}: JFace); // OK
({x: 1, y: 1, z: 1}: JFace); // OK

({}: $Required<JFace>); // ERROR
({x: 1}: $Required<JFace>); // ERROR
({x: 1, y: 1}: $Required<JFace>); // ERROR
({x: undefined, y: undefined, z: undefined}: $Required<JFace>); // ERROR

({x: 1, y: 1, z: 1}: $Required<JFace>); // OK

// No change
({x: 1, y: 1}: $Required<{x: number, y: number}>); // OK
