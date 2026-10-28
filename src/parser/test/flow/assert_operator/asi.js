// A line terminator separates postfix assertions from prefix negation.
value
!other; // OK
value
!check(); // OK
value /*
*/ !other; // OK
value?.prop
!other; // OK
value?.prop
!check(); // OK
value?.call()
!other; // OK
value?.prop /*
*/ !other; // OK

// Same-line trivia does not prevent a postfix assertion.
value /* no line terminator */ !; // OK
value?.prop /* no line terminator */ !; // OK
value?.prop /* no line terminator */ !.next; // OK
value?.call /* no line terminator */ !(); // OK

// Assertions followed by a new line still allow ordinary continuations.
value!
.prop; // OK
value?.prop!
.next; // OK
value?.call!
(); // OK

// Newlines inside a token are not ASI boundaries before an assertion.
`a
b`!; // OK
`a
b`!.length; // OK
`a${1}b
c`!; // OK
"a\
b"!; // OK
`a
b`
!other; // OK

// Comparison fallback retains the assertion when < is not a type argument list.
value?.prop! < other; // OK
value?.prop!! < other; // OK
value?.prop! /* comparison */ < other; // OK
value?.prop! <= other; // OK
value?.prop! << other; // OK
value?.prop! > other; // OK

// Prefix negation and inequality remain standard JavaScript.
!other; // OK
!!other; // OK
value != other; // OK
value !== other; // OK
value; !other; // OK
