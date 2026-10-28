// Postfix non-null assertions require type parsing.
value!; // ERROR
value?.prop!; // ERROR
value?.prop!.next; // ERROR
value?.call!(); // ERROR

// Newline negation remains valid JavaScript with types disabled.
value
!other; // OK
value?.prop
!check(); // OK
