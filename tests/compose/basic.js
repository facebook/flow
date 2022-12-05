// @flow

declare var compose: $Compose;
declare var composeReverse: $ComposeReverse;

(compose((n: number) => n.toString())(42): empty); // Error: string ~> empty

(composeReverse((n: number) => n.toString())(42): empty); // Error: string ~> empty

(compose(
  (n: number) => n * 5, // Error: string cannot be multiplied.
  (n: number) => n.toString(),
)(42): empty); // Error: number ~> empty

(composeReverse(
  (n: number) => n * 5, // OK
  (n: number) => n.toString(),
)(42): empty); // Error: string ~> empty
