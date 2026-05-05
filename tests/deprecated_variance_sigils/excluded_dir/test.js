// This file is in the excludes list — `+` and `-` variance sigils are
// accepted without error here, even though `experimental.deprecated_variance_sigils`
// is set globally.

type CovObj = { +p: number }; // ok
type ContraObj = { -p: number }; // ok

class A<+T> {} // ok
class B<-T> {} // ok
