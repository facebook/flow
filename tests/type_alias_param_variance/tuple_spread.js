// Variance is checked through the elements a tuple type spread evaluates to.

type Covariant<out X> = [string, ...[X]]; // ERROR: X in an input/output position
type ReadOnlyCovariant<out X> = Readonly<[string, ...[X]]>; // OK
type Contravariant<in X> = [...[readonly x: X], string]; // ERROR: spread elements are writable
