// @flow

// from sam, https://github.com/facebook/flow/issues/780
// call to f() within if should properly havoc x.
//
function example(b: boolean): number {
  var x = 0;
  function f() { x = "" }
  if (b) {
    f();
  }
  return x; 
}
