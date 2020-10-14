//@flow

function f<X: {...}>(ex: $Exact<X>, x: X) {
  (ex: X);
  (ex: $Exact<X>);
  (x: $Exact<X>); // nope
  (((42: any): {||}): $Exact<X>); // nope
}
