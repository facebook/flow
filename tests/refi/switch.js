// @flow
function foo(a,b,c) {
  switch (c) {
  case a.x.y: // OK
  case b.x.y: // OK
    return;
  default:
    return;
  }
}
