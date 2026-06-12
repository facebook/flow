// This file is in the excludes list, so `+` and `-` variance sigils are accepted
// without error here.

type CovObj = { +p: number }; // ok
type ContraObj = { -p: number }; // ok

class A<+T> {} // ok
class B<-T> {} // ok
