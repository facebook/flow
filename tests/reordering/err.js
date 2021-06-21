//@flow

function f() {
  g()
}
function g() {
  f()
}


function fy(): void {
  gy()
}
function gy(): void {
  fy()
}
