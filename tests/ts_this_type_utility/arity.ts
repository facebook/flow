// `ThisType` requires exactly one type argument.

type Zero = ThisType<>; // ERROR -- too few type arguments
type Two = ThisType<number, string>; // ERROR -- too many type arguments

// Negative control: well-formed.
type One = ThisType<number>; // OK
