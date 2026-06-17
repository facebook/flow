// `object` is a nullary builtin -- it accepts no type arguments.

type Bad = object<number>; // ERROR -- not a polymorphic type

// Negative control: well-formed.
type Good = object; // OK
