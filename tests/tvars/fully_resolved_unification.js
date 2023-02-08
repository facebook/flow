// This test ensures that unification does not alter the type of fully resolved tvars
type Foo = {+foo?: $ReadOnlyArray<{|
  +bar: string,
|}>};

declare function useState<S>(
  initialState: (() => S) | S,
): S => void;

declare var v: Foo;

const setState = useState(
  v?.foo ?? []
);

let x = []; // ERROR
setState(x);

// The unification here should not "leak" to other uses of x
(x: Array<{bar: string}>); // ERROR
