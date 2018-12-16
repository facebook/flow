// @flow

({ a: 123 }: {| ['a']: number |}); // OK
({ a: 123 }: {| ['a']: string |}); // error
({ a: 123 }: {| ['b']: number |}); // error
({ a: 123 }: {| [string]: number |}); // OK
({ a: 123 }: {| [string]: string |}); // error
