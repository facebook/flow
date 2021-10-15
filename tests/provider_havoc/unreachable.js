//@flow
function foo() {
  return 42;
  // no providers because unreachable
  (function (why: number) {});
}
