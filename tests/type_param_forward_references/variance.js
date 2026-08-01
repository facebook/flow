// @flow

// Polarity checking runs with every sibling tparam in scope at once, so a
// forward reference from a bound to a later sibling is well-formed while
// variance violations in the body are still reported (check_polarity path).

type CovOk<out A extends B, out B> = () => A | B; // OK: both params are covariant and only in output positions

type CovBad<out A extends B, B> = (a: A) => B; // ERROR: `A` is covariant but used in an input position
