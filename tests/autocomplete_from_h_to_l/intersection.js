//@flow

function f<A, B, C, D>(x : { foo : A, bar : B } & { foo : C, baz : D }) {
  x.
//  ^
}
